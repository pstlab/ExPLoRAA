package it.cnr.istc.pst.exploraa;

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
import it.cnr.istc.pst.exploraa.api.Teaching;
import it.cnr.istc.pst.exploraa.api.User;
import it.cnr.istc.pst.exploraa.db.FollowEntity;
import it.cnr.istc.pst.exploraa.db.LessonEntity;
import it.cnr.istc.pst.exploraa.db.LessonModelEntity;
import it.cnr.istc.pst.exploraa.db.TeachEntity;
import it.cnr.istc.pst.exploraa.db.UserEntity;

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

    static void createLesson(final Context ctx) {
        final String name = ctx.formParam("name");
        final long teacher_id = Long.parseLong(ctx.formParam("teacher_id"));
        final long model_id = Long.parseLong(ctx.formParam("model_id"));

        LOG.info("creating new lesson {}..", name);
        final EntityManager em = App.EMF.createEntityManager();

        final LessonEntity lesson_entity = new LessonEntity();
        lesson_entity.setName(name);
        lesson_entity.setTeacher(new TeachEntity(em.find(UserEntity.class, teacher_id), lesson_entity));
        final LessonModelEntity lme = em.find(LessonModelEntity.class, model_id);
        lesson_entity.setModel(lme);

        em.getTransaction().begin();
        em.persist(lesson_entity);
        em.getTransaction().commit();

        final LessonManager lesson_manager = new LessonManager(lesson_entity.getId(), lme.getModel());
        LESSONS.put(lesson_entity.getId(), lesson_manager);

        ctx.status(201);
        em.close();
    }

    static void getLesson(final Context ctx) {
        final long lesson_id = Long.parseLong(ctx.pathParam("id"));
        LOG.info("retrieving lesson {}..", lesson_id);
        final EntityManager em = App.EMF.createEntityManager();
        final LessonEntity lesson_entity = em.find(LessonEntity.class, lesson_id);
        if (lesson_entity == null)
            throw new NotFoundResponse();

        ctx.json(toLesson(lesson_entity));
        em.close();
    }

    static void deleteLesson(final Context ctx) {
        final long lesson_id = Long.parseLong(ctx.pathParam("id"));
        LOG.info("deleting lesson {}..", lesson_id);
        final EntityManager em = App.EMF.createEntityManager();
        final LessonEntity lesson_entity = em.find(LessonEntity.class, lesson_id);
        if (lesson_entity == null)
            throw new NotFoundResponse();

        em.getTransaction().begin();
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
            LOG.info("user {} is following lesson {} with interests {}..", student_id, lesson_id, interests);
            ctx.status(204);
        } catch (final JsonProcessingException e) {
            throw new InternalServerErrorResponse(e.getMessage());
        }
    }

    static void unfollowLesson(final Context ctx) {
        final long student_id = Long.parseLong(ctx.formParam("student_id"));
        final long lesson_id = Long.parseLong(ctx.formParam("lesson_id"));
        LOG.info("user {} is unfollowing lesson {} ..", student_id, lesson_id);
        ctx.status(204);
    }

    static void playLesson(final Context ctx) {
        final long lesson_id = Long.parseLong(ctx.pathParam("id"));
        LOG.info("playing lesson {}..", lesson_id);
        ctx.status(204);
    }

    static void pauseLesson(final Context ctx) {
        final long lesson_id = Long.parseLong(ctx.pathParam("id"));
        LOG.info("pausing lesson {}..", lesson_id);
        ctx.status(204);
    }

    static void stopLesson(final Context ctx) {
        final long lesson_id = Long.parseLong(ctx.pathParam("id"));
        LOG.info("stopping lesson {}..", lesson_id);
        ctx.status(204);
    }

    static Lesson toLesson(final LessonEntity entity) {
        final LessonManager lesson_manager = LESSONS.get(entity.getId());
        return new Lesson(entity.getId(), entity.getName(), entity.getModel().getId(), lesson_manager.getTopics(),
                toTeaching(entity.getTeacher()),
                entity.getStudents().stream().map(student -> toFollowing(student))
                        .collect(Collectors.toMap(following -> following.getUser().getId(), following -> following)),
                lesson_manager.getStimuli(), lesson_manager.getTokens(), lesson_manager.getState(),
                lesson_manager.getTime());
    }

    static Teaching toTeaching(final TeachEntity entity) {
        return new Teaching(new User(entity.getTeacher().getId(), entity.getTeacher().getEmail(),
                entity.getTeacher().getFirstName(), entity.getTeacher().getLastName(), null, null, null, null,
                UserController.ONLINE.contains(entity.getTeacher().getId())), null);
    }

    static Following toFollowing(final FollowEntity entity) {
        return new Following(
                new User(entity.getStudent().getId(), entity.getStudent().getEmail(),
                        entity.getStudent().getFirstName(), entity.getStudent().getLastName(), null, null, null, null,
                        UserController.ONLINE.contains(entity.getStudent().getId())),
                null, new HashSet<>(entity.getInterests()));
    }
}