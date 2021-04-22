package it.cnr.istc.pst.exploraa;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import javax.persistence.EntityManager;
import javax.persistence.NoResultException;
import javax.persistence.TypedQuery;

import com.fasterxml.jackson.core.JsonProcessingException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.javalin.http.ConflictResponse;
import io.javalin.http.Context;
import io.javalin.http.ForbiddenResponse;
import io.javalin.http.InternalServerErrorResponse;
import io.javalin.http.NotFoundResponse;
import io.javalin.websocket.WsContext;
import it.cnr.istc.pst.exploraa.App.ExplRole;
import it.cnr.istc.pst.exploraa.api.Lesson;
import it.cnr.istc.pst.exploraa.api.LessonModel;
import it.cnr.istc.pst.exploraa.api.Message;
import it.cnr.istc.pst.exploraa.api.User;
import it.cnr.istc.pst.exploraa.db.UserEntity;

public class UserController {

    static final Logger LOG = LoggerFactory.getLogger(UserController.class);
    /**
     * For each user id, a boolean indicating whether the user is online.
     */
    static final Map<Long, WsContext> ONLINE = new HashMap<>();

    /**
     * Given an email and a password, returns the user corresponding to the email if
     * the password corresponds to the stored one.
     * 
     * @param ctx
     */
    static void login(final Context ctx) {
        final String email = ctx.formParam("email");
        final String password = ctx.formParam("password");
        LOG.info("user {} is logging in..", email);

        final EntityManager em = App.EMF.createEntityManager();
        final TypedQuery<UserEntity> query = em.createQuery("SELECT u FROM UserEntity u WHERE u.email = :email",
                UserEntity.class);
        query.setParameter("email", email);
        try {
            final UserEntity user_entity = query.getSingleResult();
            if (!App.hashPassword(password, user_entity.getSalt()).equals(user_entity.getPassword()))
                throw new ForbiddenResponse();

            ctx.json(toUser(user_entity));
        } catch (final NoResultException e) {
            throw new NotFoundResponse();
        }
        em.close();
    }

    /**
     * Returns all the stored users.
     * 
     * @param ctx
     */
    static void getAllUsers(final Context ctx) {
        LOG.info("retrieving all users..");
        final EntityManager em = App.EMF.createEntityManager();
        final List<UserEntity> user_entities = em.createQuery("SELECT ue FROM UserEntity ue", UserEntity.class)
                .getResultList();

        ctx.json(user_entities.stream().map(user -> toUser(user)).collect(Collectors.toList()));
        em.close();
    }

    /**
     * Creates a new user and stores it into the database.
     * 
     * @param ctx
     */
    static void createUser(final Context ctx) {
        final String email = ctx.formParam("email");
        final String salt = App.generateSalt();
        final String password = App.hashPassword(ctx.formParam("password"), salt);
        final String first_name = ctx.formParam("first_name");
        final String last_name = ctx.formParam("last_name");
        final String profile = ctx.formParam("profile");
        LOG.info("creating new user {}..", email);
        final EntityManager em = App.EMF.createEntityManager();

        final UserEntity user_entity = new UserEntity();
        user_entity.setEmail(email);
        user_entity.setSalt(salt);
        user_entity.setPassword(password);
        user_entity.setFirstName(first_name);
        user_entity.setLastName(last_name);
        user_entity.setProfile(profile);
        user_entity.addRole(ExplRole.User.name());

        try {
            em.getTransaction().begin();
            em.persist(user_entity);
            em.getTransaction().commit();
        } catch (final Exception ex) {
            throw new ConflictResponse();
        }

        ctx.status(201);
        em.close();
    }

    /**
     * Returns, if exists, a stored user having the given id.
     * 
     * @param ctx
     */
    static void getUser(final Context ctx) {
        final long user_id = Long.valueOf(ctx.queryParam("id"));
        LOG.info("retrieving user #{}..", user_id);
        final EntityManager em = App.EMF.createEntityManager();
        final UserEntity user_entity = em.find(UserEntity.class, user_id);
        if (user_entity == null)
            throw new NotFoundResponse();

        ctx.json(toUser(user_entity));
        em.close();
    }

    /**
     * Returns, if exists, a stored teacher having the given id.
     * 
     * @param ctx
     */
    static void getTeacher(final Context ctx) {
        final long user_id = Long.valueOf(ctx.pathParam("id"));
        LOG.info("retrieving teacher #{}..", user_id);
        final EntityManager em = App.EMF.createEntityManager();
        final UserEntity user_entity = em.find(UserEntity.class, user_id);
        if (user_entity == null)
            throw new NotFoundResponse();

        ctx.json(toTeacher(user_entity));
        em.close();
    }

    /**
     * Returns all the teachers which can be followed by the user having the given
     * id.
     * 
     * @param ctx
     */
    static void getFollowableTeachers(final Context ctx) {
        final long user_id = Long.valueOf(ctx.pathParam("id"));
        LOG.info("retrieving available teachers for user #{}..", user_id);
        final EntityManager em = App.EMF.createEntityManager();
        final UserEntity user_entity = em.find(UserEntity.class, user_id);
        if (user_entity == null)
            throw new NotFoundResponse();
        final List<UserEntity> user_entities = em.createQuery("SELECT ue FROM UserEntity ue", UserEntity.class)
                .getResultList();
        user_entities.remove(user_entity);
        user_entities.removeAll(user_entity.getTeachers());

        ctx.json(user_entities.stream().map(user -> toTeacher(user)).collect(Collectors.toList()));
        em.close();
    }

    static void getStudent(final Context ctx) {
        final long user_id = Long.valueOf(ctx.pathParam("id"));
        LOG.info("retrieving student #{}..", user_id);
        final EntityManager em = App.EMF.createEntityManager();
        final UserEntity user_entity = em.find(UserEntity.class, user_id);
        if (user_entity == null)
            throw new NotFoundResponse();

        ctx.json(toStudent(user_entity));
        em.close();
    }

    static void updateUser(final Context ctx) {
        final long user_id = Long.valueOf(ctx.pathParam("id"));
        LOG.info("updating user #{}..", user_id);
        final User user = ctx.bodyAsClass(User.class);

        final EntityManager em = App.EMF.createEntityManager();
        final UserEntity user_entity = em.find(UserEntity.class, user_id);
        if (user_entity == null)
            throw new NotFoundResponse();

        em.getTransaction().begin();
        user_entity.setFirstName(user.getFirstName());
        user_entity.setLastName(user.getLastName());
        user_entity.setProfile(user.getProfile());
        em.getTransaction().commit();

        try {
            for (UserEntity teacher : user_entity.getTeachers()) {
                if (ONLINE.containsKey(teacher.getId())) {
                    LOG.info("communicating the unfollowing to teacher #{}..", teacher.getId());
                    ONLINE.get(teacher.getId())
                            .send(App.MAPPER.writeValueAsString(new Message.ProfileUpdate(user_id, user.getProfile())));
                }
            }
        } catch (final JsonProcessingException ex) {
            LOG.error(ex.getMessage(), ex);
            throw new InternalServerErrorResponse(ex.getMessage());
        }

        ctx.status(204);
        em.close();
    }

    static void deleteUser(final Context ctx) {
        final long user_id = Long.valueOf(ctx.pathParam("id"));
        LOG.info("deleting user #{}..", user_id);
        final EntityManager em = App.EMF.createEntityManager();
        final UserEntity user_entity = em.find(UserEntity.class, user_id);
        if (user_entity == null)
            throw new NotFoundResponse();

        em.getTransaction().begin();
        user_entity.getFollowingLessons().stream().forEach(l -> {
            l.removeStudent(user_entity);
            if (ONLINE.containsKey(l.getTeacher().getId())) {
                // we notify the teacher that a student is not following a lesson anymore..
                try {
                    ONLINE.get(l.getTeacher().getId()).send(
                            App.MAPPER.writeValueAsString(new Message.UnfollowLesson(user_entity.getId(), l.getId())));
                } catch (JsonProcessingException ex) {
                    LOG.error(ex.getMessage(), ex);
                    throw new InternalServerErrorResponse(ex.getMessage());
                }
            }
        });
        user_entity.getTeachingLessons().stream().forEach(l -> {
            l.getStudents().stream().forEach(s -> {
                s.removeFollowingLesson(l);
                if (ONLINE.containsKey(s.getId())) {
                    // we notify the student that a lesson cannot be followed anymore..
                    try {
                        ONLINE.get(s.getId()).send(App.MAPPER.writeValueAsString(new Message.RemoveLesson(l.getId())));
                    } catch (JsonProcessingException ex) {
                        LOG.error(ex.getMessage(), ex);
                        throw new InternalServerErrorResponse(ex.getMessage());
                    }
                }
            });
            em.remove(l);
        });
        user_entity.getStudents().stream().forEach(s -> {
            s.removeTeacher(user_entity);
            if (ONLINE.containsKey(s.getId())) {
                // TODO: notify the student that a teacher cannot be followed anymore..
            }
        });
        user_entity.getTeachers().stream().forEach(t -> {
            t.removeStudent(user_entity);
            if (ONLINE.containsKey(t.getId())) {
                // we notify the teacher that a student is not following anymore..
                try {
                    ONLINE.get(t.getId()).send(App.MAPPER.writeValueAsString(new Message.Follower(user_id, false)));
                } catch (JsonProcessingException e) {
                    LOG.error(e.getMessage(), e);
                }
            }
        });
        em.remove(user_entity);
        em.getTransaction().commit();

        try {
            for (UserEntity teacher : user_entity.getTeachers()) {
                if (ONLINE.containsKey(teacher.getId())) {
                    LOG.info("communicating the unfollowing to teacher #{}..", teacher.getId());
                    ONLINE.get(teacher.getId())
                            .send(App.MAPPER.writeValueAsString(new Message.Follower(user_id, false)));
                }
            }
        } catch (final JsonProcessingException ex) {
            LOG.error(ex.getMessage(), ex);
            throw new InternalServerErrorResponse(ex.getMessage());
        }

        ctx.status(204);
        em.close();
    }

    static void followUser(final Context ctx) {
        final long student_id = Long.valueOf(ctx.queryParam("student_id"));
        final long teacher_id = Long.valueOf(ctx.queryParam("teacher_id"));
        LOG.info("student #{} is following teacher #{}..", student_id, teacher_id);
        final EntityManager em = App.EMF.createEntityManager();
        final UserEntity student_entity = em.find(UserEntity.class, student_id);
        if (student_entity == null)
            throw new NotFoundResponse();
        final UserEntity teacher_entity = em.find(UserEntity.class, teacher_id);
        if (teacher_entity == null)
            throw new NotFoundResponse();

        em.getTransaction().begin();
        student_entity.addTeacher(teacher_entity);
        teacher_entity.addStudent(student_entity);
        em.getTransaction().commit();

        if (ONLINE.containsKey(teacher_id))
            try {
                LOG.info("communicating the following to teacher #{}..", teacher_id);
                ONLINE.get(teacher_id).send(App.MAPPER.writeValueAsString(new Message.Follower(student_id, true)));
            } catch (final JsonProcessingException ex) {
                LOG.error(ex.getMessage(), ex);
                throw new InternalServerErrorResponse(ex.getMessage());
            }

        ctx.status(204);
        em.close();
    }

    static void unfollowUser(final Context ctx) {
        final long student_id = Long.valueOf(ctx.queryParam("student_id"));
        final long teacher_id = Long.valueOf(ctx.queryParam("teacher_id"));
        LOG.info("student #{} is unfollowing teacher #{}..", student_id, teacher_id);
        final EntityManager em = App.EMF.createEntityManager();
        final UserEntity student_entity = em.find(UserEntity.class, student_id);
        if (student_entity == null)
            throw new NotFoundResponse();
        final UserEntity teacher_entity = em.find(UserEntity.class, teacher_id);
        if (teacher_entity == null)
            throw new NotFoundResponse();

        em.getTransaction().begin();
        student_entity.removeTeacher(teacher_entity);
        teacher_entity.removeStudent(student_entity);
        em.getTransaction().commit();

        if (ONLINE.containsKey(teacher_id))
            try {
                LOG.info("communicating the unfollowing to teacher #{}..", teacher_id);
                ONLINE.get(teacher_id).send(App.MAPPER.writeValueAsString(new Message.Follower(student_id, false)));
            } catch (final JsonProcessingException ex) {
                LOG.error(ex.getMessage(), ex);
                throw new InternalServerErrorResponse(ex.getMessage());
            }

        ctx.status(204);
        em.close();
    }

    static User toUser(final UserEntity entity) {
        final boolean online = ONLINE.containsKey(entity.getId());

        final Map<Long, User> teachers = entity.getTeachers().stream().map(t -> toTeacher(t))
                .collect(Collectors.toMap(t -> t.getId(), t -> t));
        final Map<Long, User> students = entity.getStudents().stream().map(s -> toStudent(s))
                .collect(Collectors.toMap(s -> s.getId(), s -> s));
        final Map<Long, Lesson> following_lessons = entity.getFollowingLessons().stream()
                .map(l -> LessonController.toFollowing(l)).collect(Collectors.toMap(l -> l.getId(), l -> l));
        final Map<Long, Lesson> teaching_lessons = entity.getTeachingLessons().stream()
                .map(l -> LessonController.toTeaching(l)).collect(Collectors.toMap(l -> l.getId(), l -> l));
        final Map<Long, LessonModel> models = entity.getModels().stream().map(m -> LessonController.toModel(m))
                .collect(Collectors.toMap(m -> m.getId(), m -> m));

        return new User(entity.getId(), entity.getEmail(), entity.getFirstName(), entity.getLastName(),
                entity.getProfile(), teachers, students, following_lessons, teaching_lessons, models, online);
    }

    static User toTeacher(final UserEntity entity) {
        return new User(entity.getId(), entity.getEmail(), entity.getFirstName(), entity.getLastName(), null, null,
                null, null, null, null, ONLINE.containsKey(entity.getId()));
    }

    static User toStudent(final UserEntity entity) {
        return new User(entity.getId(), entity.getEmail(), entity.getFirstName(), entity.getLastName(),
                entity.getProfile(), null, null, null, null, null, ONLINE.containsKey(entity.getId()));
    }
}
