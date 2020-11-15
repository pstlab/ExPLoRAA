package it.cnr.istc.pst.exploraa;

import static io.javalin.apibuilder.ApiBuilder.delete;
import static io.javalin.apibuilder.ApiBuilder.get;
import static io.javalin.apibuilder.ApiBuilder.path;
import static io.javalin.apibuilder.ApiBuilder.post;
import static io.javalin.core.security.SecurityUtil.roles;

import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.security.spec.InvalidKeySpecException;
import java.util.Arrays;
import java.util.Base64;
import java.util.List;
import java.util.Set;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;

import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.PBEKeySpec;
import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;

import com.fasterxml.jackson.databind.ObjectMapper;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.javalin.Javalin;
import io.javalin.core.security.Role;
import io.javalin.http.Context;
import io.javalin.http.Handler;
import io.javalin.http.UnauthorizedResponse;
import it.cnr.istc.pst.exploraa.db.LessonEntity;
import it.cnr.istc.pst.exploraa.db.UserEntity;

/**
 * Hello world!
 *
 */
public class App {

    static final Logger LOG = LoggerFactory.getLogger(UserController.class);
    static final Executor EXECUTOR = Executors.newSingleThreadExecutor();
    static final EntityManagerFactory EMF = Persistence.createEntityManagerFactory("ExPLoRAA_PU");
    static final ObjectMapper MAPPER = new ObjectMapper();
    private static final SecureRandom RAND = new SecureRandom();
    // private static final int ITERATIONS = 65536;
    private static final int ITERATIONS = 5;
    private static final int KEY_LENGTH = 512;
    private static final String ALGORITHM = "PBKDF2WithHmacSHA512";

    public static void main(String[] args) {
        final Javalin app = Javalin.create(config -> {
            config.addStaticFiles("/public");
            config.accessManager((final Handler handler, final Context ctx, final Set<Role> permittedRoles) -> {
                if (permittedRoles.contains(getRole(ctx)))
                    handler.handle(ctx);
                else
                    throw new UnauthorizedResponse();
            });
            config.enableCorsForAllOrigins();
        });

        app.events(event -> {
            event.serverStarting(() -> LOG.info("Starting ExPLoRAA server.."));
            event.serverStarted(() -> {
                LOG.info("ExPLoRAA server is running..");
                final EntityManager em = App.EMF.createEntityManager();

                final List<UserEntity> users = em.createQuery("SELECT ue FROM UserEntity ue", UserEntity.class)
                        .getResultList();

                LOG.info("Loading {} users..", users.size());

                final List<LessonEntity> lessons = em.createQuery("SELECT le FROM LessonEntity le", LessonEntity.class)
                        .getResultList();

                LOG.info("Loading {} lessons..", lessons.size());
                // warning! we do not store the current time of the lesson, nor its state
                // if the service is restarted, the lesson is not lost, yet its state is!
                for (final LessonEntity l_entity : lessons)
                    LessonController.LESSONS.put(l_entity.getId(),
                            new LessonManager(l_entity.getId(), l_entity.getModel().getModel()));
            });
        });

        // we create the routes..
        app.routes(() -> {
            post("login", UserController::login, roles(ExplRole.Guest, ExplRole.Admin));
            path("users", () -> {
                get(UserController::getAllUsers, roles(ExplRole.Admin, ExplRole.User));
                post(UserController::createUser, roles(ExplRole.Guest, ExplRole.Admin));
                path(":id", () -> {
                    get(UserController::getUser, roles(ExplRole.Admin, ExplRole.User));
                    post(UserController::updateUser, roles(ExplRole.Admin, ExplRole.User));
                    delete(UserController::deleteUser, roles(ExplRole.Admin, ExplRole.User));
                });
            });
        });

        app.ws("/communication", ws -> {
            ws.onConnect(ctx -> {
                EXECUTOR.execute(() -> {
                    final Long id = Long.valueOf(ctx.queryParam("id"));
                    LOG.info("User #{} connected..", id);
                    UserController.ONLINE.put(id, ctx);
                    final EntityManager em = App.EMF.createEntityManager();
                    final UserEntity user_entity = em.find(UserEntity.class, id);
                });
            });
            ws.onClose(ctx -> {
                EXECUTOR.execute(() -> {
                    final Long id = Long.valueOf(ctx.queryParam("id"));
                    LOG.info("User #{} disconnected..", id);
                    UserController.ONLINE.remove(id);
                });
            });
            ws.onMessage(ctx -> EXECUTOR.execute(() -> LOG.info("Received message {}..", ctx)));
        }, roles(ExplRole.Admin, ExplRole.User));

        app.start();
    }

    static Role getRole(final Context ctx) {
        final String auth_head = ctx.header("Authorization");
        Long id = null;
        if (auth_head != null)
            id = Long.valueOf(auth_head.replace("Basic ", ""));
        final String ws_protocol_head = ctx.header("Sec-WebSocket-Protocol");
        if (ws_protocol_head != null)
            id = Long.valueOf(ctx.queryParam("id"));
        if (id == null) {
            final EntityManager em = App.EMF.createEntityManager();
            final UserEntity user_entity = em.find(UserEntity.class, id);
            em.close();
            if (user_entity == null)
                return ExplRole.Guest;
            else if (user_entity.getRoles().contains(ExplRole.Admin.name()))
                return ExplRole.Admin;
            else
                return ExplRole.User;
        }
        return ExplRole.Guest;
    }

    public static String generateSalt() {
        byte[] salt = new byte[KEY_LENGTH];
        RAND.nextBytes(salt);
        return Base64.getEncoder().encodeToString(salt);
    }

    public static String hashPassword(String password, String salt) {
        char[] chars = password.toCharArray();
        byte[] bytes = salt.getBytes();

        PBEKeySpec spec = new PBEKeySpec(chars, bytes, ITERATIONS, KEY_LENGTH);

        Arrays.fill(chars, Character.MIN_VALUE);

        try {
            SecretKeyFactory fac = SecretKeyFactory.getInstance(ALGORITHM);
            byte[] securePassword = fac.generateSecret(spec).getEncoded();
            return Base64.getEncoder().encodeToString(securePassword);
        } catch (NoSuchAlgorithmException | InvalidKeySpecException ex) {
            return null;
        } finally {
            spec.clearPassword();
        }
    }

    enum ExplRole implements Role {
        Guest, User, Admin
    }
}
