package it.cnr.istc.pst.exploraa.server;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import javax.persistence.EntityManager;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.javalin.http.Context;
import io.javalin.http.InternalServerErrorResponse;
import io.javalin.http.NotFoundResponse;
import it.cnr.istc.pst.exploraa.api.Following;
import it.cnr.istc.pst.exploraa.api.Lesson;
import it.cnr.istc.pst.exploraa.api.LessonModel;
import it.cnr.istc.pst.exploraa.api.LessonModel.TextStimulusTemplate;
import it.cnr.istc.pst.exploraa.api.LessonModel.URLStimulusTemplate;
import it.cnr.istc.pst.exploraa.api.Teaching;
import it.cnr.istc.pst.exploraa.api.User;
import it.cnr.istc.pst.exploraa.server.db.FollowEntity;
import it.cnr.istc.pst.exploraa.server.db.LessonEntity;
import it.cnr.istc.pst.exploraa.server.db.LessonModelEntity;
import it.cnr.istc.pst.exploraa.server.db.TeachEntity;
import it.cnr.istc.pst.exploraa.server.db.UserEntity;
import it.cnr.istc.pst.exploraa.server.solver.LessonManager;

/**
 * LessonController
 */
public class LessonController {

    static final Logger LOG = LoggerFactory.getLogger(LessonController.class);
    /**
     * For each lesson, the context of the lesson.
     */
    static final Map<Long, LessonManager> LESSONS = new HashMap<>();

    static void getAllLessons(Context ctx) {
        LOG.info("retrieving all lessons..");
        EntityManager em = App.EMF.createEntityManager();
        List<LessonEntity> lesson_entities = em.createQuery("SELECT le FROM LessonEntity le", LessonEntity.class)
                .getResultList();
        try {
            List<Lesson> lessons = new ArrayList<>(lesson_entities.size());
            for (LessonEntity lesson_entity : lesson_entities) {
                LessonModel lm = App.MAPPER.readValue(lesson_entity.getModel().getModel(), LessonModel.class);

                Lesson lesson = new Lesson(lesson_entity.getId(), lesson_entity.getName(), lm.getId(), getTopics(lm),
                        toTeaching(lesson_entity.getTeacher()),
                        lesson_entity.getStudents().stream().map(student -> toFollowing(student)).collect(
                                Collectors.toMap(following -> following.getUser().getId(), following -> following)),
                        null, 0);

                lessons.add(lesson);
            }

            ctx.json(lessons);
        } catch (Exception e) {
            throw new InternalServerErrorResponse(e.getMessage());
        }
    }

    static void createLesson(Context ctx) {
        String name = ctx.formParam("name");
        long teacher_id = Long.parseLong(ctx.formParam("teacher_id"));
        long model_id = Long.parseLong(ctx.formParam("model_id"));

        LOG.info("creating new lesson {}..", name);
        EntityManager em = App.EMF.createEntityManager();

        LessonEntity lesson_entity = new LessonEntity();
        lesson_entity.setName(name);
        lesson_entity.setTeacher(new TeachEntity(em.find(UserEntity.class, teacher_id), lesson_entity));
        LessonModelEntity lme = em.find(LessonModelEntity.class, model_id);
        lesson_entity.setModel(lme);

        em.getTransaction().begin();
        em.persist(lesson_entity);
        em.getTransaction().commit();

        try {
            LessonModel lm = App.MAPPER.readValue(lme.getModel(), LessonModel.class);
            Lesson l = new Lesson(lesson_entity.getId(), lesson_entity.getName(), model_id, getTopics(lm),
                    toTeaching(lesson_entity.getTeacher()),
                    lesson_entity.getStudents().stream().map(student -> toFollowing(student)).collect(
                            Collectors.toMap(following -> following.getUser().getId(), following -> following)),
                    null, 0);
            LESSONS.put(l.getId(), new LessonManager(l));
        } catch (JsonProcessingException e) {
            throw new InternalServerErrorResponse(e.getMessage());
        }

        ctx.status(201);
    }

    static void getLesson(Context ctx) {
        long lesson_id = Long.parseLong(ctx.pathParam("id"));
        LOG.info("retrieving lesson {}..", lesson_id);
        EntityManager em = App.EMF.createEntityManager();
        LessonEntity lesson_entity = em.find(LessonEntity.class, lesson_id);
        if (lesson_entity == null)
            throw new NotFoundResponse();

        try {
            LessonModel lm = App.MAPPER.readValue(lesson_entity.getModel().getModel(), LessonModel.class);

            Lesson lesson = new Lesson(lesson_entity.getId(), lesson_entity.getName(), lm.getId(), getTopics(lm),
                    toTeaching(lesson_entity.getTeacher()),
                    lesson_entity.getStudents().stream().map(student -> toFollowing(student)).collect(
                            Collectors.toMap(following -> following.getUser().getId(), following -> following)),
                    null, 0);
            ctx.json(lesson);
        } catch (Exception e) {
            throw new InternalServerErrorResponse(e.getMessage());
        }
    }

    static void deleteLesson(Context ctx) {
        long lesson_id = Long.parseLong(ctx.pathParam("id"));
        LOG.info("deleting lesson {}..", lesson_id);
        EntityManager em = App.EMF.createEntityManager();
        LessonEntity lesson_entity = em.find(LessonEntity.class, lesson_id);
        if (lesson_entity == null)
            throw new NotFoundResponse();

        em.getTransaction().begin();
        em.remove(lesson_entity);
        em.getTransaction().commit();
        ctx.status(204);
    }

    static void followLesson(Context ctx) {
        try {
            long student_id = Long.parseLong(ctx.formParam("student_id"));
            long lesson_id = Long.parseLong(ctx.formParam("lesson_id"));
            Set<String> interests = App.MAPPER.readValue(ctx.formParam("lesson_id"), new TypeReference<Set<String>>() {
            });
            LOG.info("user {} is following lesson {} with interests {}..", student_id, lesson_id, interests);
            ctx.status(204);
        } catch (JsonProcessingException e) {
            throw new InternalServerErrorResponse(e.getMessage());
        }
    }

    static void unfollowLesson(Context ctx) {
        long student_id = Long.parseLong(ctx.formParam("student_id"));
        long lesson_id = Long.parseLong(ctx.formParam("lesson_id"));
        LOG.info("user {} is unfollowing lesson {} ..", student_id, lesson_id);
        ctx.status(204);
    }

    static void playLesson(Context ctx) {
        long lesson_id = Long.parseLong(ctx.pathParam("id"));
        LOG.info("playing lesson {}..", lesson_id);
        ctx.status(204);
    }

    static void pauseLesson(Context ctx) {
        long lesson_id = Long.parseLong(ctx.pathParam("id"));
        LOG.info("pausing lesson {}..", lesson_id);
        ctx.status(204);
    }

    static void stopLesson(Context ctx) {
        long lesson_id = Long.parseLong(ctx.pathParam("id"));
        LOG.info("stopping lesson {}..", lesson_id);
        ctx.status(204);
    }

    static Set<String> getTopics(LessonModel model) {
        return model.getStimuli().values().stream().flatMap(template -> {
            if (template instanceof TextStimulusTemplate) {
                TextStimulusTemplate txt = (TextStimulusTemplate) template;
                return (txt.getTopics() != null) ? txt.getTopics().stream() : new HashSet<String>().stream();
            } else if (template instanceof URLStimulusTemplate) {
                URLStimulusTemplate url = (URLStimulusTemplate) template;
                return (url.getTopics() != null) ? url.getTopics().stream() : new HashSet<String>().stream();
            } else
                throw new AssertionError(template.getClass().getName());
        }).collect(Collectors.toSet());
    }

    static Teaching toTeaching(TeachEntity entity) {
        return new Teaching(new User(entity.getTeacher().getId(), entity.getTeacher().getEmail(),
                entity.getTeacher().getFirstName(), entity.getTeacher().getLastName(), null, null, null, null,
                UserController.ONLINE.contains(entity.getTeacher().getId())), null);
    }

    static Following toFollowing(FollowEntity entity) {
        return new Following(
                new User(entity.getStudent().getId(), entity.getStudent().getEmail(),
                        entity.getStudent().getFirstName(), entity.getStudent().getLastName(), null, null, null, null,
                        UserController.ONLINE.contains(entity.getStudent().getId())),
                null, new HashSet<>(entity.getInterests()));
    }
}