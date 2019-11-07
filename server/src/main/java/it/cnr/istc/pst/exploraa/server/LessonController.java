package it.cnr.istc.pst.exploraa.server;

import java.util.ArrayList;
import java.util.List;

import javax.persistence.EntityManager;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.javalin.http.Context;
import io.javalin.http.NotFoundResponse;
import it.cnr.istc.pst.exploraa.api.Lesson;
import it.cnr.istc.pst.exploraa.server.db.LessonEntity;
import it.cnr.istc.pst.exploraa.server.db.LessonModelEntity;
import it.cnr.istc.pst.exploraa.server.db.TeachEntity;
import it.cnr.istc.pst.exploraa.server.db.UserEntity;

/**
 * LessonController
 */
public class LessonController {

    static final Logger LOG = LoggerFactory.getLogger(LessonController.class);

    static public void getAllLessons(Context ctx) {
        LOG.info("retrieving all lessons..");
        EntityManager em = App.EMF.createEntityManager();
        List<LessonEntity> lesson_entities = em.createQuery("SELECT le FROM LessonEntity le", LessonEntity.class)
                .getResultList();

        List<Lesson> lessons = new ArrayList<>(lesson_entities.size());
        for (LessonEntity lesson_entity : lesson_entities)
            lessons.add(new Lesson(lesson_entity.getId(), lesson_entity.getName(), lesson_entity.getModel().getId(),
                    null, null));

        ctx.json(lessons);
    }

    static public void createLesson(Context ctx) {
        String name = ctx.formParam("name");
        long teacher_id = Long.parseLong(ctx.formParam("teacher_id"));
        long model_id = Long.parseLong(ctx.formParam("model_id"));

        LOG.info("creating new lesson {}..", name);
        EntityManager em = App.EMF.createEntityManager();

        LessonEntity lesson_entity = new LessonEntity();
        lesson_entity.setName(name);
        lesson_entity.setTeacher(new TeachEntity(em.find(UserEntity.class, teacher_id), lesson_entity));
        lesson_entity.setModel(em.find(LessonModelEntity.class, model_id));

        em.getTransaction().begin();
        em.persist(lesson_entity);
        em.getTransaction().commit();
        ctx.status(201);
    }

    static public void getLesson(Context ctx) {
        LOG.info("retrieving lesson {}..", ctx.pathParam("id"));
        EntityManager em = App.EMF.createEntityManager();
        LessonEntity lesson_entity = em.find(LessonEntity.class, Long.valueOf(ctx.pathParam("id")));
        if (lesson_entity == null)
            throw new NotFoundResponse();

        Lesson lesson = new Lesson(lesson_entity.getId(), lesson_entity.getName(), lesson_entity.getModel().getId(),
                null, null);
        ctx.json(lesson);
    }

    static public void deleteLesson(Context ctx) {
        LOG.info("deleting lesson {}..", ctx.pathParam("id"));
        EntityManager em = App.EMF.createEntityManager();
        LessonEntity lesson_entity = em.find(LessonEntity.class, Long.valueOf(ctx.pathParam("id")));
        if (lesson_entity == null)
            throw new NotFoundResponse();

        em.getTransaction().begin();
        em.remove(lesson_entity);
        em.getTransaction().commit();
        ctx.status(204);
    }
}