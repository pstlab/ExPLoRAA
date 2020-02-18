package it.cnr.istc.pst.exploraa.server;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.persistence.EntityManager;
import javax.persistence.NoResultException;
import javax.persistence.TypedQuery;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.javalin.http.ConflictResponse;
import io.javalin.http.Context;
import io.javalin.http.ForbiddenResponse;
import io.javalin.http.NotFoundResponse;
import it.cnr.istc.pst.exploraa.api.Parameter;
import it.cnr.istc.pst.exploraa.api.User;
import it.cnr.istc.pst.exploraa.server.App.ExplRole;
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

    static void login(Context ctx) {
        String email = ctx.formParam("email");
        String password = ctx.formParam("password");
        LOG.info("user {} is logging in..", email);

        EntityManager em = App.EMF.createEntityManager();
        TypedQuery<UserEntity> query = em.createQuery(
                "SELECT u FROM UserEntity u WHERE u.email = :email AND u.password = :password", UserEntity.class);
        query.setParameter("email", email);
        query.setParameter("password", password);
        try {
            UserEntity user_entity = query.getSingleResult();

            User user = new User(user_entity.getId(), user_entity.getEmail(), user_entity.getFirstName(),
                    user_entity.getLastName(), null, null, null, null, ONLINE.contains(user_entity.getId()));
            ctx.json(user);
        } catch (NoResultException e) {
            throw new ForbiddenResponse();
        }
    }

    static void getAllUsers(Context ctx) {
        LOG.info("retrieving all users..");
        EntityManager em = App.EMF.createEntityManager();
        List<UserEntity> user_entities = em.createQuery("SELECT ue FROM UserEntity ue", UserEntity.class)
                .getResultList();

        List<User> users = new ArrayList<>(user_entities.size());
        for (UserEntity user_entity : user_entities)
            users.add(new User(user_entity.getId(), user_entity.getEmail(), user_entity.getFirstName(),
                    user_entity.getLastName(), PARAMETER_TYPES.get(user_entity.getId()),
                    PARAMETER_VALUES.get(user_entity.getId()), null, null, ONLINE.contains(user_entity.getId())));

        ctx.json(users);
    }

    static void createUser(Context ctx) {
        String email = ctx.formParam("email");
        String password = ctx.formParam("password");
        String first_name = ctx.formParam("first_name");
        String last_name = ctx.formParam("last_name");
        LOG.info("creating new user {}..", email);
        EntityManager em = App.EMF.createEntityManager();

        UserEntity user_entity = new UserEntity();
        user_entity.setEmail(email);
        user_entity.setPassword(password);
        user_entity.setFirstName(first_name);
        user_entity.setLastName(last_name);
        user_entity.addRole(ExplRole.User.name());

        try {
            em.getTransaction().begin();
            em.persist(user_entity);
            em.getTransaction().commit();
            ctx.status(201);
        } catch (Exception ex) {
            throw new ConflictResponse();
        }
    }

    static void getUser(Context ctx) {
        long user_id = Long.valueOf(ctx.pathParam("id"));
        LOG.info("retrieving user {}..", user_id);
        EntityManager em = App.EMF.createEntityManager();
        UserEntity user_entity = em.find(UserEntity.class, user_id);
        if (user_entity == null)
            throw new NotFoundResponse();

        User user = new User(user_entity.getId(), user_entity.getEmail(), user_entity.getFirstName(),
                user_entity.getLastName(), PARAMETER_TYPES.get(user_entity.getId()),
                PARAMETER_VALUES.get(user_entity.getId()), null, null, ONLINE.contains(user_entity.getId()));
        ctx.json(user);
    }

    static void updateUser(Context ctx) {
        long user_id = Long.valueOf(ctx.pathParam("id"));
        LOG.info("updating user {}..", user_id);
        User user = ctx.bodyAsClass(User.class);

        EntityManager em = App.EMF.createEntityManager();
        UserEntity user_entity = em.find(UserEntity.class, user_id);
        if (user_entity == null)
            throw new NotFoundResponse();

        em.getTransaction().begin();
        user_entity.setFirstName(user.getFirstName());
        user_entity.setLastName(user.getLastName());
        em.getTransaction().commit();
        ctx.status(204);
    }

    static void deleteUser(Context ctx) {
        long user_id = Long.valueOf(ctx.pathParam("id"));
        LOG.info("deleting user {}..", user_id);
        EntityManager em = App.EMF.createEntityManager();
        UserEntity user_entity = em.find(UserEntity.class, user_id);
        if (user_entity == null)
            throw new NotFoundResponse();

        em.getTransaction().begin();
        em.remove(user_entity);
        em.getTransaction().commit();

        PARAMETER_TYPES.remove(user_entity.getId());
        PARAMETER_VALUES.remove(user_entity.getId());
        ctx.status(204);
    }
}