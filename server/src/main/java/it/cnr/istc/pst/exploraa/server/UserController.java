package it.cnr.istc.pst.exploraa.server;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.persistence.EntityManager;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.javalin.http.Context;
import io.javalin.http.NotFoundResponse;
import it.cnr.istc.pst.exploraa.api.User;
import it.cnr.istc.pst.exploraa.server.db.UserEntity;

/**
 * UserController
 */
public class UserController {

    static final Logger LOG = LoggerFactory.getLogger(UserController.class);
    /**
     * For each user id, a boolean indicating whether the user is online.
     */
    static final Map<Long, Boolean> ONLINE = new HashMap<>();

    static public void login(Context ctx) {
        String email = ctx.cookie("email");
        String password = ctx.cookie("password");
        LOG.info("login user {}..", email);
    }

    static public void getAllUsers(Context ctx) {
        LOG.info("retrieving all users..");
        EntityManager em = App.EMF.createEntityManager();
        List<UserEntity> user_entities = em.createQuery("SELECT ue FROM UserEntity ue", UserEntity.class)
                .getResultList();

        List<User> users = new ArrayList<>(user_entities.size());
        for (UserEntity user_entity : user_entities)
            users.add(new User(user_entity.getId(), user_entity.getEmail(), user_entity.getFirstName(),
                    user_entity.getLastName(), ONLINE.getOrDefault(user_entity.getId(), false)));

        ctx.json(users);
    }

    static public void createUser(Context ctx) {
        LOG.info("creating new user..");
        EntityManager em = App.EMF.createEntityManager();

        UserEntity user_entity = new UserEntity();
        user_entity.setEmail(ctx.formParam("email"));
        user_entity.setPassword(ctx.formParam("password"));

        em.getTransaction().begin();
        em.persist(user_entity);
        em.getTransaction().commit();
    }

    static public void getUser(Context ctx) {
        LOG.info("retrieving user {}..", ctx.pathParam("id"));
        EntityManager em = App.EMF.createEntityManager();
        UserEntity user_entity = em.find(UserEntity.class, Long.valueOf(ctx.pathParam("id")));
        if (user_entity == null)
            throw new NotFoundResponse();

        ctx.json(new User(user_entity.getId(), user_entity.getEmail(), user_entity.getFirstName(),
                user_entity.getLastName(), ONLINE.getOrDefault(user_entity.getId(), false)));
    }

    static public void updateUser(Context ctx) {
        LOG.info("updating user {}..", ctx.pathParam("id"));
        User user = ctx.bodyAsClass(User.class);

        EntityManager em = App.EMF.createEntityManager();
        UserEntity user_entity = em.find(UserEntity.class, Long.valueOf(ctx.pathParam("id")));
        if (user_entity == null)
            throw new NotFoundResponse();

        em.getTransaction().begin();
        user_entity.setFirstName(user.getFirstName());
        user_entity.setLastName(user.getLastName());
        em.getTransaction().commit();
    }

    static public void deleteUser(Context ctx) {
        LOG.info("deleting user {}..", ctx.pathParam("id"));
        EntityManager em = App.EMF.createEntityManager();
        UserEntity user_entity = em.find(UserEntity.class, Long.valueOf(ctx.pathParam("id")));
        if (user_entity == null)
            throw new NotFoundResponse();

        em.getTransaction().begin();
        em.remove(user_entity);
        em.getTransaction().commit();
    }
}