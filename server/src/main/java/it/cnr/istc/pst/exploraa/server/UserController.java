package it.cnr.istc.pst.exploraa.server;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import javax.persistence.EntityManager;
import javax.persistence.NoResultException;
import javax.persistence.TypedQuery;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.javalin.http.ConflictResponse;
import io.javalin.http.Context;
import io.javalin.http.ForbiddenResponse;
import io.javalin.http.NotFoundResponse;
import it.cnr.istc.pst.exploraa.api.Following;
import it.cnr.istc.pst.exploraa.api.Parameter;
import it.cnr.istc.pst.exploraa.api.Teaching;
import it.cnr.istc.pst.exploraa.api.User;
import it.cnr.istc.pst.exploraa.server.RESTService.ExplRole;
import it.cnr.istc.pst.exploraa.server.db.UserEntity;

/**
 * UserController
 */
public class UserController {

    static final Logger LOG = LoggerFactory.getLogger(UserController.class);
    /**
     * For each user id, a boolean indicating whether the user is online.
     */
    static final Set<Long> ONLINE = new HashSet<>();
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
        final String password = App.hashPassword(ctx.formParam("password"), App.generateSalt(512));
        LOG.info("user {} is logging in..", email);

        final EntityManager em = App.EMF.createEntityManager();
        final TypedQuery<UserEntity> query = em.createQuery(
                "SELECT u FROM UserEntity u WHERE u.email = :email AND u.password = :password", UserEntity.class);
        query.setParameter("email", email);
        query.setParameter("password", password);
        try {
            ctx.json(toUser(query.getSingleResult()));
        } catch (final NoResultException e) {
            throw new ForbiddenResponse();
        }
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
        final String password = App.hashPassword(ctx.formParam("password"), App.generateSalt(512));
        final String first_name = ctx.formParam("first_name");
        final String last_name = ctx.formParam("last_name");
        LOG.info("creating new user {}..", email);
        final EntityManager em = App.EMF.createEntityManager();

        final UserEntity user_entity = new UserEntity();
        user_entity.setEmail(email);
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
        final long user_id = Long.valueOf(ctx.pathParam("id"));
        LOG.info("retrieving user {}..", user_id);
        final EntityManager em = App.EMF.createEntityManager();
        final UserEntity user_entity = em.find(UserEntity.class, user_id);
        if (user_entity == null)
            throw new NotFoundResponse();

        ctx.json(toUser(user_entity));
        em.close();
    }

    static void updateUser(final Context ctx) {
        final long user_id = Long.valueOf(ctx.pathParam("id"));
        LOG.info("updating user {}..", user_id);
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
        final long user_id = Long.valueOf(ctx.pathParam("id"));
        LOG.info("deleting user {}..", user_id);
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

    static User toUser(final UserEntity entity) {
        final boolean online = ONLINE.contains(entity.getId());
        final Map<String, Parameter> par_types = online ? PARAMETER_TYPES.get(entity.getId()) : null;
        final Map<String, Map<String, String>> par_vals = online ? PARAMETER_VALUES.get(entity.getId()) : null;
        final List<Following> following = entity.getFollowedLessons().stream().map(l -> LessonController.toFollowing(l))
                .collect(Collectors.toList());
        final List<Teaching> teaching = entity.getTeachedLessons().stream().map(l -> LessonController.toTeaching(l))
                .collect(Collectors.toList());

        return new User(entity.getId(), entity.getEmail(), entity.getFirstName(), entity.getLastName(), par_types,
                par_vals, following, teaching, online);
    }
}