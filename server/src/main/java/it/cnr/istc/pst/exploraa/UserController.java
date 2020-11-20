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
import io.javalin.http.NotFoundResponse;
import io.javalin.websocket.WsContext;
import it.cnr.istc.pst.exploraa.App.ExplRole;
import it.cnr.istc.pst.exploraa.api.Following;
import it.cnr.istc.pst.exploraa.api.FollowingLesson;
import it.cnr.istc.pst.exploraa.api.Message;
import it.cnr.istc.pst.exploraa.api.Parameter;
import it.cnr.istc.pst.exploraa.api.TeachingLesson;
import it.cnr.istc.pst.exploraa.api.User;
import it.cnr.istc.pst.exploraa.db.FollowingEntity;
import it.cnr.istc.pst.exploraa.db.FollowingId;
import it.cnr.istc.pst.exploraa.db.UserEntity;

public class UserController {

    static final Logger LOG = LoggerFactory.getLogger(UserController.class);
    /**
     * For each user id, a boolean indicating whether the user is online.
     */
    static final Map<Long, WsContext> ONLINE = new HashMap<>();
    /**
     * For each user id, a map of parameter types containing the name of the
     * parameter as key.
     */
    static final Map<Long, Map<String, Parameter>> PARAMETER_TYPES = new HashMap<>();
    /**
     * For each user id, a map of parameter values containing the name of the
     * parameter as key. Notice that parameter values are represented through a map.
     */
    static final Map<Long, Map<String, Map<String, String>>> PARAMETER_VALUES = new HashMap<>();

    static void login(final Context ctx) {
        final String email = ctx.formParam("email");
        final String password = ctx.formParam("password");
        LOG.info("user {} is logging in..", email);

        final EntityManager em = App.EMF.createEntityManager();
        final TypedQuery<UserEntity> query = em.createQuery("SELECT u FROM UserEntity u WHERE u.email = :email",
                UserEntity.class);
        query.setParameter("email", email);
        UserEntity user_entity = null;
        try {
            user_entity = query.getSingleResult();
        } catch (final NoResultException e) {
            throw new ForbiddenResponse();
        }
        if (!App.hashPassword(password, user_entity.getSalt()).equals(user_entity.getPassword()))
            throw new ForbiddenResponse();

        ctx.json(toUser(user_entity));
        em.close();
    }

    static void getAllUsers(final Context ctx) {
        LOG.info("retrieving all users..");
        final EntityManager em = App.EMF.createEntityManager();
        final List<UserEntity> user_entities = em.createQuery("SELECT ue FROM UserEntity ue", UserEntity.class)
                .getResultList();

        ctx.json(user_entities.stream().map(user -> toUser(user)).collect(Collectors.toList()));
        em.close();
    }

    static void createUser(final Context ctx) {
        final String email = ctx.formParam("email");
        final String salt = App.generateSalt();
        final String password = App.hashPassword(ctx.formParam("password"), salt);
        final String first_name = ctx.formParam("first_name");
        final String last_name = ctx.formParam("last_name");
        LOG.info("creating new user {}..", email);
        final EntityManager em = App.EMF.createEntityManager();

        final UserEntity user_entity = new UserEntity();
        user_entity.setEmail(email);
        user_entity.setSalt(salt);
        user_entity.setPassword(password);
        user_entity.setFirstName(first_name);
        user_entity.setLastName(last_name);
        user_entity.addRole(ExplRole.User.name());

        try {
            em.getTransaction().begin();
            em.persist(user_entity);
            em.getTransaction().commit();
        } catch (final Exception ex) {
            throw new ConflictResponse();
        }

        PARAMETER_TYPES.put(user_entity.getId(), new HashMap<>());
        PARAMETER_VALUES.put(user_entity.getId(), new HashMap<>());

        ctx.status(201);
        em.close();
    }

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

    static void getTeacher(final Context ctx) {
        final long user_id = Long.valueOf(ctx.queryParam("id"));
        LOG.info("retrieving teacher #{}..", user_id);
        final EntityManager em = App.EMF.createEntityManager();
        final UserEntity user_entity = em.find(UserEntity.class, user_id);
        if (user_entity == null)
            throw new NotFoundResponse();

        ctx.json(toTeacher(user_entity));
        em.close();
    }

    static void getStudent(final Context ctx) {
        final long user_id = Long.valueOf(ctx.queryParam("id"));
        LOG.info("retrieving student #{}..", user_id);
        final EntityManager em = App.EMF.createEntityManager();
        final UserEntity user_entity = em.find(UserEntity.class, user_id);
        if (user_entity == null)
            throw new NotFoundResponse();

        ctx.json(toStudent(user_entity));
        em.close();
    }

    static void updateUser(final Context ctx) {
        final long user_id = Long.valueOf(ctx.queryParam("id"));
        LOG.info("updating user #{}..", user_id);
        final User user = ctx.bodyAsClass(User.class);

        final EntityManager em = App.EMF.createEntityManager();
        final UserEntity user_entity = em.find(UserEntity.class, user_id);
        if (user_entity == null)
            throw new NotFoundResponse();

        em.getTransaction().begin();
        user_entity.setFirstName(user.getFirstName());
        user_entity.setLastName(user.getLastName());
        em.getTransaction().commit();

        ctx.status(204);
        em.close();
    }

    static void deleteUser(final Context ctx) {
        final long user_id = Long.valueOf(ctx.queryParam("id"));
        LOG.info("deleting user #{}..", user_id);
        final EntityManager em = App.EMF.createEntityManager();
        final UserEntity user_entity = em.find(UserEntity.class, user_id);
        if (user_entity == null)
            throw new NotFoundResponse();

        em.getTransaction().begin();
        em.remove(user_entity);
        em.getTransaction().commit();

        PARAMETER_TYPES.remove(user_entity.getId());
        PARAMETER_VALUES.remove(user_entity.getId());

        ctx.status(204);
        em.close();
    }

    static void followUser(final Context ctx) {
        final long student_id = Long.valueOf(ctx.queryParam("student_id"));
        final long teacher_id = Long.valueOf(ctx.queryParam("teacher_id"));
        final EntityManager em = App.EMF.createEntityManager();
        final UserEntity student_entity = em.find(UserEntity.class, student_id);
        if (student_entity == null)
            throw new NotFoundResponse();
        final UserEntity teacher_entity = em.find(UserEntity.class, teacher_id);
        if (teacher_entity == null)
            throw new NotFoundResponse();

        em.getTransaction().begin();
        FollowingEntity following_entity = new FollowingEntity();
        following_entity.setStudent(student_entity);
        following_entity.setTeacher(teacher_entity);
        student_entity.addTeacher(following_entity);
        teacher_entity.addStudent(following_entity);
        em.persist(following_entity);
        em.getTransaction().commit();

        if (ONLINE.containsKey(teacher_id))
            try {
                ONLINE.get(teacher_id).send(App.MAPPER.writeValueAsString(new Message.Follower(student_id, true)));
            } catch (JsonProcessingException e) {
                LOG.error(e.getMessage(), e);
            }

        ctx.status(204);
        em.close();
    }

    static void unfollowUser(final Context ctx) {
        final long student_id = Long.valueOf(ctx.queryParam("student_id"));
        final long teacher_id = Long.valueOf(ctx.queryParam("teacher_id"));
        final EntityManager em = App.EMF.createEntityManager();
        final UserEntity student_entity = em.find(UserEntity.class, student_id);
        if (student_entity == null)
            throw new NotFoundResponse();
        final UserEntity teacher_entity = em.find(UserEntity.class, teacher_id);
        if (teacher_entity == null)
            throw new NotFoundResponse();
        FollowingEntity following_entity = em.find(FollowingEntity.class, new FollowingId(student_id, teacher_id));
        if (following_entity == null)
            throw new NotFoundResponse();

        em.getTransaction().begin();
        student_entity.removeTeacher(following_entity);
        teacher_entity.removeStudent(following_entity);
        em.remove(following_entity);
        em.getTransaction().commit();

        if (ONLINE.containsKey(teacher_id))
            try {
                ONLINE.get(teacher_id).send(App.MAPPER.writeValueAsString(new Message.Follower(student_id, false)));
            } catch (JsonProcessingException e) {
                LOG.error(e.getMessage(), e);
            }

        ctx.status(204);
        em.close();
    }

    static User toUser(final UserEntity entity) {
        final boolean online = ONLINE.containsKey(entity.getId());
        final Map<String, Parameter> par_types = online ? PARAMETER_TYPES.get(entity.getId()) : null;
        final Map<String, Map<String, String>> par_vals = online ? PARAMETER_VALUES.get(entity.getId()) : null;
        final List<Following> teachers = entity.getTeachers().stream()
                .map(l -> new Following(null, toTeacher(l.getTeacher()))).collect(Collectors.toList());
        final List<Following> students = entity.getStudents().stream()
                .map(l -> new Following(toStudent(l.getStudent()), null)).collect(Collectors.toList());
        final List<FollowingLesson> following_lessons = entity.getFollowingLessons().stream()
                .map(l -> LessonController.toFollowing(l)).collect(Collectors.toList());
        final List<TeachingLesson> teaching_lessons = entity.getTeachingLessons().stream()
                .map(l -> LessonController.toTeaching(l)).collect(Collectors.toList());

        return new User(entity.getId(), entity.getEmail(), entity.getFirstName(), entity.getLastName(), par_types,
                par_vals, teachers, students, following_lessons, teaching_lessons, online);
    }

    static User toTeacher(final UserEntity entity) {
        return new User(entity.getId(), entity.getEmail(), entity.getFirstName(), entity.getLastName(), null, null,
                null, null, null, null, ONLINE.containsKey(entity.getId()));
    }

    static User toStudent(final UserEntity entity) {
        return new User(entity.getId(), entity.getEmail(), entity.getFirstName(), entity.getLastName(), null, null,
                null, null, null, null, ONLINE.containsKey(entity.getId()));
    }
}
