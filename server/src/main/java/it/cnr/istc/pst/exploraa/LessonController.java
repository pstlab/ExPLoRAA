package it.cnr.istc.pst.exploraa;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

import javax.persistence.EntityManager;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.JsonNode;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.javalin.http.Context;
import io.javalin.http.InternalServerErrorResponse;
import io.javalin.http.NotFoundResponse;
import it.cnr.istc.pst.exploraa.api.Lesson;
import it.cnr.istc.pst.exploraa.api.LessonModel;
import it.cnr.istc.pst.exploraa.api.Message;
import it.cnr.istc.pst.exploraa.api.User;
import it.cnr.istc.pst.exploraa.db.LessonEntity;
import it.cnr.istc.pst.exploraa.db.ModelEntity;
import it.cnr.istc.pst.exploraa.db.RuleEntity;
import it.cnr.istc.pst.exploraa.db.TextRuleEntity;
import it.cnr.istc.pst.exploraa.db.UserEntity;
import it.cnr.istc.pst.exploraa.db.WebRuleEntity;
import it.cnr.istc.pst.exploraa.db.WikiRuleEntity;

public class LessonController {

    static final Logger LOG = LoggerFactory.getLogger(LessonController.class);
    /**
     * For each lesson, the context of the lesson.
     */
    static final Map<Long, LessonManager> LESSONS = new HashMap<>();

    static void getAllLessons(final Context ctx) {
        LOG.info("retrieving all lessons..");
        final EntityManager em = App.EMF.createEntityManager();
        final List<LessonEntity> lesson_entities = em.createQuery("SELECT le FROM LessonEntity le", LessonEntity.class)
                .getResultList();

        ctx.json(lesson_entities.stream().map(lesson -> toLesson(lesson)).collect(Collectors.toList()));
        em.close();
    }

    static void getFollowableLessons(final Context ctx) {
        final long user_id = Long.valueOf(ctx.pathParam("id"));
        LOG.info("retrieving followable lessons for user #{}..", user_id);
        final EntityManager em = App.EMF.createEntityManager();
        final UserEntity user_entity = em.find(UserEntity.class, user_id);
        if (user_entity == null)
            throw new NotFoundResponse();

        final Map<Long, LessonEntity> lessons = user_entity.getTeachers().stream()
                .flatMap(tchr -> tchr.getTeachingLessons().stream())
                .collect(Collectors.toMap(LessonEntity::getId, Function.identity()));
        user_entity.getFollowingLessons().stream().map(l -> l.getId()).forEach(id -> lessons.remove(id));

        ctx.json(lessons.values().stream().map(l -> toLesson(l)).collect(Collectors.toList()));
        em.close();
    }

    static void createModel(final Context ctx) {
        final String name = ctx.formParam("name");
        final long teacher_id = Long.parseLong(ctx.formParam("teacher_id"));

        LOG.info("creating new model {}..", name);
        final ModelEntity model = new ModelEntity();
        model.setName(name);

        final EntityManager em = App.EMF.createEntityManager();

        final UserEntity teacher_entity = em.find(UserEntity.class, teacher_id);

        em.getTransaction().begin();
        teacher_entity.addModel(model);
        model.addTeacher(teacher_entity);
        em.persist(model);

        em.getTransaction().commit();

        ctx.json(toModel(model));
        em.close();
    }

    static void getModel(final Context ctx) {
        final long model_id = Long.parseLong(ctx.pathParam("id"));
        LOG.info("retrieving lesson #{}..", model_id);
        final EntityManager em = App.EMF.createEntityManager();
        final ModelEntity model_entity = em.find(ModelEntity.class, model_id);
        if (model_entity == null)
            throw new NotFoundResponse();

        ctx.json(toModel(model_entity));
        em.close();
    }

    static void deleteModel(final Context ctx) {
        final long model_id = Long.parseLong(ctx.pathParam("id"));
        LOG.info("deleting model #{}..", model_id);
        final EntityManager em = App.EMF.createEntityManager();
        final ModelEntity model_entity = em.find(ModelEntity.class, model_id);
        if (model_entity == null)
            throw new NotFoundResponse();

        em.getTransaction().begin();
        model_entity.getTeachers().stream().forEach(t -> t.removeModel(model_entity));
        em.remove(model_entity);
        em.getTransaction().commit();

        ctx.status(204);
        em.close();
    }

    static void createRule(final Context ctx) {
        final long model_id = Long.parseLong(ctx.formParam("model_id"));
        final String name = ctx.formParam("name");
        final String type = ctx.formParam("type");
        final String effect_id = ctx.formParam("effect_id");

        final EntityManager em = App.EMF.createEntityManager();
        final ModelEntity model_entity = em.find(ModelEntity.class, model_id);
        if (model_entity == null)
            throw new NotFoundResponse();

        em.getTransaction().begin();
        RuleEntity rule_entity = null;
        switch (type) {
        case "text":
            rule_entity = new TextRuleEntity();
            ((TextRuleEntity) rule_entity).setText(ctx.formParam("text"));
            break;
        case "web":
            rule_entity = new WebRuleEntity();
            ((WebRuleEntity) rule_entity).setUrl(ctx.formParam("url"));
            break;
        case "wiki":
            rule_entity = new WikiRuleEntity();

            try {
                JsonNode wiki = App.WCB_CLIENT.wiki(name, "1");
                ((WikiRuleEntity) rule_entity).setUrl(wiki.get("url").asText());
                for (JsonNode cat : wiki.get("categories"))
                    rule_entity.addTopic(cat.asText());
                rule_entity.setLength(wiki.get("length").asLong());
                for (JsonNode pre : wiki.get("preconditions"))
                    rule_entity.addSuggestion(pre.asText());
            } catch (JsonProcessingException ex) {
                LOG.error("Cannot invoke WCB ", ex);
                throw new InternalServerErrorResponse(ex.getMessage());
            }
            break;
        default:
            break;
        }

        if (effect_id != null) {
            final RuleEntity effect_entity = em.find(RuleEntity.class, Long.parseLong(effect_id));
            if (effect_entity == null)
                throw new NotFoundResponse();
            rule_entity.addEffect(effect_entity);
            effect_entity.addPrecondition(rule_entity);
        }

        rule_entity.setName(name);
        em.persist(rule_entity);
        model_entity.addRule(rule_entity);
        em.getTransaction().commit();

        ctx.json(toRule(rule_entity));
        em.close();
    }

    static void updateRule(final Context ctx) {
        try {
            final long rule_id = Long.parseLong(ctx.pathParam("id"));
            final long length = Long.parseLong(ctx.formParam("length"));
            final String[] topics = App.MAPPER.readValue(ctx.formParam("topics"), String[].class);

            LOG.info("updating rule #{}..", rule_id);
            final EntityManager em = App.EMF.createEntityManager();
            final RuleEntity rule_entity = em.find(RuleEntity.class, rule_id);
            if (rule_entity == null)
                throw new NotFoundResponse();

            em.getTransaction().begin();
            rule_entity.setLength(length);
            for (String topic : topics) {
                rule_entity.addTopic(topic);
            }
            em.getTransaction().commit();

            ctx.status(204);
            em.close();
        } catch (JsonProcessingException ex) {
            LOG.error(ex.getMessage(), ex);
            throw new InternalServerErrorResponse(ex.getMessage());
        }
    }

    static void deleteRule(final Context ctx) {
        final long rule_id = Long.parseLong(ctx.pathParam("id"));
        LOG.info("deleting rule #{}..", rule_id);
        final EntityManager em = App.EMF.createEntityManager();
        final RuleEntity rule_entity = em.find(RuleEntity.class, rule_id);
        if (rule_entity == null)
            throw new NotFoundResponse();

        em.getTransaction().begin();
        em.remove(rule_entity);
        em.getTransaction().commit();

        ctx.status(204);
        em.close();
    }

    static void createPrecondition(final Context ctx) {
        final long condition_id = Long.parseLong(ctx.formParam("condition_id"));
        final long effect_id = Long.parseLong(ctx.formParam("effect_id"));

        final EntityManager em = App.EMF.createEntityManager();
        final RuleEntity condition_entity = em.find(RuleEntity.class, condition_id);
        if (condition_entity == null)
            throw new NotFoundResponse();
        final RuleEntity effect_entity = em.find(RuleEntity.class, effect_id);
        if (effect_entity == null)
            throw new NotFoundResponse();

        em.getTransaction().begin();
        condition_entity.addEffect(effect_entity);
        effect_entity.addPrecondition(condition_entity);
        em.getTransaction().commit();

        ctx.status(204);
        em.close();
    }

    static void deletePrecondition(final Context ctx) {
        final long condition_id = Long.parseLong(ctx.formParam("condition_id"));
        final long effect_id = Long.parseLong(ctx.formParam("effect_id"));

        LOG.info("deleting precondition #{} from effect #{}..", new Object[] { condition_id, effect_id });
        final EntityManager em = App.EMF.createEntityManager();
        final RuleEntity condition_entity = em.find(RuleEntity.class, condition_id);
        if (condition_entity == null)
            throw new NotFoundResponse();
        final RuleEntity effect_entity = em.find(RuleEntity.class, effect_id);
        if (effect_entity == null)
            throw new NotFoundResponse();

        em.getTransaction().begin();
        condition_entity.removeEffect(effect_entity);
        effect_entity.removePrecondition(condition_entity);
        em.getTransaction().commit();

        ctx.status(204);
        em.close();
    }

    static void createLesson(final Context ctx) {
        final String name = ctx.formParam("name");
        final long teacher_id = Long.parseLong(ctx.formParam("teacher_id"));
        final long model_id = Long.parseLong(ctx.formParam("model_id"));
        long[] students_ids = null;
        long[] goals_ids = null;
        try {
            students_ids = App.MAPPER.readValue(ctx.formParam("students_ids"), long[].class);
            goals_ids = App.MAPPER.readValue(ctx.formParam("goals_ids"), long[].class);
        } catch (JsonProcessingException ex) {
            LOG.error(ex.getMessage(), ex);
            throw new InternalServerErrorResponse(ex.getMessage());
        }

        LOG.info("creating new lesson {}..", name);
        final LessonEntity lesson_entity = new LessonEntity();
        lesson_entity.setName(name);

        final EntityManager em = App.EMF.createEntityManager();

        final ModelEntity lme = em.find(ModelEntity.class, model_id);
        final UserEntity teacher_entity = em.find(UserEntity.class, teacher_id);

        em.getTransaction().begin();
        lesson_entity.setModel(lme);

        lesson_entity.setTeacher(teacher_entity);
        teacher_entity.addTeachingLesson(lesson_entity);

        for (int i = 0; i < students_ids.length; i++) {
            final UserEntity student_entity = em.find(UserEntity.class, students_ids[i]);
            lesson_entity.addStudent(student_entity);
            student_entity.addFollowingLesson(lesson_entity);
        }

        for (int i = 0; i < goals_ids.length; i++)
            lesson_entity.addGoal(em.find(RuleEntity.class, goals_ids[i]));

        em.persist(lesson_entity);
        em.getTransaction().commit();

        final LessonManager lesson_manager = new LessonManager(lesson_entity);
        LESSONS.put(lesson_entity.getId(), lesson_manager);

        ctx.status(201);
        em.close();
    }

    static void getLesson(final Context ctx) {
        final long lesson_id = Long.parseLong(ctx.pathParam("id"));
        LOG.info("retrieving lesson #{}..", lesson_id);
        final EntityManager em = App.EMF.createEntityManager();
        final LessonEntity lesson_entity = em.find(LessonEntity.class, lesson_id);
        if (lesson_entity == null)
            throw new NotFoundResponse();

        ctx.json(toLesson(lesson_entity));
        em.close();
    }

    static void deleteLesson(final Context ctx) {
        final long lesson_id = Long.parseLong(ctx.pathParam("id"));
        LOG.info("deleting lesson #{}..", lesson_id);
        final EntityManager em = App.EMF.createEntityManager();
        final LessonEntity lesson_entity = em.find(LessonEntity.class, lesson_id);
        if (lesson_entity == null)
            throw new NotFoundResponse();

        em.getTransaction().begin();
        lesson_entity.getStudents().stream().forEach(s -> {
            s.removeFollowingLesson(lesson_entity);
            if (UserController.ONLINE.containsKey(s.getId())) {
                // we notify the student that a lesson cannot be followed anymore..
                try {
                    UserController.ONLINE.get(s.getId())
                            .send(App.MAPPER.writeValueAsString(new Message.RemoveLesson(lesson_id)));
                } catch (JsonProcessingException ex) {
                    LOG.error(ex.getMessage(), ex);
                    throw new InternalServerErrorResponse(ex.getMessage());
                }
            }
        });
        em.remove(lesson_entity);
        em.getTransaction().commit();

        ctx.status(204);
        em.close();
    }

    static void followLesson(final Context ctx) {
        try {
            final long student_id = Long.parseLong(ctx.formParam("student_id"));
            final long lesson_id = Long.parseLong(ctx.formParam("lesson_id"));
            final Set<String> interests = App.MAPPER.readValue(ctx.formParam("interests"),
                    new TypeReference<Set<String>>() {
                    });
            LOG.info("user #{} is following lesson #{} with interests {}..", student_id, lesson_id, interests);
            ctx.status(204);
        } catch (final JsonProcessingException ex) {
            throw new InternalServerErrorResponse(ex.getMessage());
        }
    }

    static void unfollowLesson(final Context ctx) {
        final long student_id = Long.parseLong(ctx.formParam("student_id"));
        final long lesson_id = Long.parseLong(ctx.formParam("lesson_id"));
        LOG.info("user #{} is unfollowing lesson #{} ..", student_id, lesson_id);
        ctx.status(204);
    }

    static void playLesson(final Context ctx) {
        final long lesson_id = Long.parseLong(ctx.pathParam("id"));
        LOG.info("playing lesson #{}..", lesson_id);
        ctx.status(204);
    }

    static void pauseLesson(final Context ctx) {
        final long lesson_id = Long.parseLong(ctx.pathParam("id"));
        LOG.info("pausing lesson #{}..", lesson_id);
        ctx.status(204);
    }

    static void stopLesson(final Context ctx) {
        final long lesson_id = Long.parseLong(ctx.pathParam("id"));
        LOG.info("stopping lesson #{}..", lesson_id);
        ctx.status(204);
    }

    static Lesson toLesson(final LessonEntity entity) {
        final LessonManager lesson_manager = LESSONS.get(entity.getId());
        final Map<Long, User> students = entity.getStudents().stream().map(student -> UserController.toStudent(student))
                .collect(Collectors.toMap(student -> student.getId(), student -> student));
        return new Lesson(entity.getId(), entity.getName(), entity.getModel().getId(), lesson_manager.getTopics(),
                UserController.toTeacher(entity.getTeacher()), students, lesson_manager.getStimuli(),
                lesson_manager.getTokens(), lesson_manager.getState(), lesson_manager.getTime());
    }

    static Lesson toTeaching(final LessonEntity entity) {
        final LessonManager lesson_manager = LESSONS.get(entity.getId());
        final Map<Long, User> students = entity.getStudents().stream().map(student -> UserController.toStudent(student))
                .collect(Collectors.toMap(student -> student.getId(), student -> student));
        return new Lesson(entity.getId(), entity.getName(), entity.getModel().getId(), lesson_manager.getTopics(), null,
                students, lesson_manager.getStimuli(), lesson_manager.getTokens(), lesson_manager.getState(),
                lesson_manager.getTime());
    }

    static Lesson toFollowing(final LessonEntity entity) {
        final LessonManager lesson_manager = LESSONS.get(entity.getId());
        return new Lesson(entity.getId(), entity.getName(), entity.getModel().getId(), lesson_manager.getTopics(),
                UserController.toTeacher(entity.getTeacher()), null, lesson_manager.getStimuli(),
                lesson_manager.getTokens(), lesson_manager.getState(), lesson_manager.getTime());
    }

    static LessonModel toModel(final ModelEntity entity) {
        return new LessonModel(entity.getId(), entity.getName(),
                entity.getRules().stream().map(stimulus -> toRule(stimulus))
                        .collect(Collectors.toMap(stimulus -> stimulus.getId(), stimulus -> stimulus)));
    }

    static LessonModel.Rule toRule(final RuleEntity entity) {
        if (entity instanceof TextRuleEntity) {
            final TextRuleEntity text_rule_entity = (TextRuleEntity) entity;
            return new LessonModel.Rule.WebRule(entity.getId(), entity.getName(), entity.getTopics(),
                    entity.getLength(), entity.isTopDown(),
                    entity.getPreconditions().stream().map(pre -> pre.getId()).collect(Collectors.toSet()),
                    entity.getSuggestions(), text_rule_entity.getText());
        } else if (entity instanceof WebRuleEntity) {
            final WebRuleEntity web_rule_entity = (WebRuleEntity) entity;
            return new LessonModel.Rule.WebRule(entity.getId(), entity.getName(), entity.getTopics(),
                    entity.getLength(), entity.isTopDown(),
                    entity.getPreconditions().stream().map(pre -> pre.getId()).collect(Collectors.toSet()),
                    entity.getSuggestions(), web_rule_entity.getUrl());
        } else if (entity instanceof WikiRuleEntity) {
            final WikiRuleEntity wiki_rule_entity = (WikiRuleEntity) entity;
            return new LessonModel.Rule.WikiRule(entity.getId(), entity.getName(), entity.getTopics(),
                    entity.getLength(), entity.isTopDown(),
                    entity.getPreconditions().stream().map(pre -> pre.getId()).collect(Collectors.toSet()),
                    entity.getSuggestions(), wiki_rule_entity.getUrl());
        }
        return null;
    }
}
