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
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.PBEKeySpec;
import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.javalin.Javalin;
import io.javalin.core.security.Role;
import io.javalin.http.Context;
import io.javalin.http.Handler;
import io.javalin.http.UnauthorizedResponse;
import io.javalin.websocket.WsContext;
import it.cnr.istc.pst.exploraa.api.Message;
import it.cnr.istc.pst.exploraa.db.LessonEntity;
import it.cnr.istc.pst.exploraa.db.UserEntity;

public class App {

    static final Logger LOG = LoggerFactory.getLogger(App.class);
    static final EntityManagerFactory EMF = Persistence.createEntityManagerFactory("ExPLoRAA_PU");
    static final ObjectMapper MAPPER = new ObjectMapper();
    private static final SecureRandom RAND = new SecureRandom();
    // private static final int ITERATIONS = 65536;
    private static final int ITERATIONS = 5;
    private static final int KEY_LENGTH = 512;
    private static final String ALGORITHM = "PBKDF2WithHmacSHA512";

    public static void main(final String[] args) {
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
                    LessonController.LESSONS.put(l_entity.getId(), new LessonManager(l_entity));
            });
        });

        // we create the routes..
        app.routes(() -> {
            post("login", UserController::login, roles(ExplRole.Guest, ExplRole.Admin));
            path("user", () -> {
                post(UserController::createUser, roles(ExplRole.Guest, ExplRole.Admin));
                path("follow", () -> post(UserController::followUser, roles(ExplRole.Admin, ExplRole.User)));
                path("unfollow", () -> post(UserController::unfollowUser, roles(ExplRole.Admin, ExplRole.User)));
                path(":id", () -> {
                    get(UserController::getUser, roles(ExplRole.Admin, ExplRole.User));
                    post(UserController::updateUser, roles(ExplRole.Admin, ExplRole.User));
                    delete(UserController::deleteUser, roles(ExplRole.Admin, ExplRole.User));
                });
            });
            path("users", () -> get(UserController::getAllUsers, roles(ExplRole.Admin)));
            path("teacher/:id", () -> get(UserController::getTeacher, roles(ExplRole.Admin, ExplRole.User)));
            path("teachers/:id",
                    () -> get(UserController::getFollowableTeachers, roles(ExplRole.Admin, ExplRole.User)));
            path("student/:id", () -> get(UserController::getStudent, roles(ExplRole.Admin, ExplRole.User)));
            path("lesson", () -> {
                post(LessonController::createLesson, roles(ExplRole.Admin, ExplRole.User));
                path("follow", () -> post(LessonController::followLesson, roles(ExplRole.Admin, ExplRole.User)));
                path("unfollow", () -> post(LessonController::unfollowLesson, roles(ExplRole.Admin, ExplRole.User)));
                path("play/:id", () -> post(LessonController::playLesson, roles(ExplRole.Admin, ExplRole.User)));
                path("pause/:id", () -> post(LessonController::pauseLesson, roles(ExplRole.Admin, ExplRole.User)));
                path("stop/:id", () -> post(LessonController::stopLesson, roles(ExplRole.Admin, ExplRole.User)));
                path(":id", () -> {
                    get(LessonController::getLesson, roles(ExplRole.Admin, ExplRole.User));
                    delete(LessonController::deleteLesson, roles(ExplRole.Admin, ExplRole.User));
                });
            });
            path("lessons", () -> {
                get(LessonController::getAllLessons, roles(ExplRole.Admin));
                path(":id", () -> get(LessonController::getFollowableLessons, roles(ExplRole.Admin, ExplRole.User)));
            });
        });

        app.ws("/communication", ws -> {
            ws.onConnect(ctx -> new_connection(ctx));
            ws.onClose(ctx -> lost_connection(ctx));
            ws.onMessage(ctx -> new_message(ctx));
        }, roles(ExplRole.Admin, ExplRole.User));

        app.start();

        Runtime.getRuntime().addShutdownHook(new Thread(() -> {
            app.stop();
        }));
    }

    private static synchronized void new_connection(final WsContext ctx) {
        final Long id = Long.valueOf(ctx.queryParam("id"));
        LOG.info("User #{} connected..", id);
        UserController.ONLINE.put(id, ctx);
        final EntityManager em = App.EMF.createEntityManager();
        final UserEntity user_entity = em.find(UserEntity.class, id);

        final Set<WsContext> ctxs = new HashSet<>();
        ctxs.addAll(user_entity.getTeachers().stream().map(teacher -> teacher.getId())
                .filter(teacher_id -> UserController.ONLINE.containsKey(teacher_id))
                .map(teacher_id -> UserController.ONLINE.get(teacher_id)).collect(Collectors.toSet()));
        ctxs.addAll(user_entity.getStudents().stream().map(student -> student.getId())
                .filter(student_id -> UserController.ONLINE.containsKey(student_id))
                .map(student_id -> UserController.ONLINE.get(student_id)).collect(Collectors.toSet()));

        try {
            for (final WsContext ws_ctx : ctxs) {
                ws_ctx.send(MAPPER.writeValueAsString(new Message.Online(id, true)));
            }
        } catch (final JsonProcessingException e) {
            LOG.error(e.getMessage(), e);
        }
    }

    private static synchronized void lost_connection(final WsContext ctx) {
        final Long id = Long.valueOf(ctx.queryParam("id"));
        LOG.info("User #{} disconnected..", id);
        UserController.ONLINE.remove(id);
        final EntityManager em = App.EMF.createEntityManager();
        final UserEntity user_entity = em.find(UserEntity.class, id);

        final Set<WsContext> ctxs = new HashSet<>();
        ctxs.addAll(user_entity.getTeachers().stream().map(teacher -> teacher.getId())
                .filter(teacher_id -> UserController.ONLINE.containsKey(teacher_id))
                .map(teacher_id -> UserController.ONLINE.get(teacher_id)).collect(Collectors.toSet()));
        ctxs.addAll(user_entity.getStudents().stream().map(student -> student.getId())
                .filter(student_id -> UserController.ONLINE.containsKey(student_id))
                .map(student_id -> UserController.ONLINE.get(student_id)).collect(Collectors.toSet()));

        try {
            for (final WsContext ws_ctx : ctxs) {
                ws_ctx.send(MAPPER.writeValueAsString(new Message.Online(id, false)));
            }
        } catch (final JsonProcessingException e) {
            LOG.error(e.getMessage(), e);
        }
    }

    private static synchronized void new_message(final WsContext ctx) {
        LOG.info("Received message {}..", ctx);
    }

    static Role getRole(final Context ctx) {
        final String auth_head = ctx.header("Authorization");
        Long id = null;
        if (auth_head != null)
            id = Long.valueOf(auth_head.replace("Basic ", ""));
        final String ws_protocol_head = ctx.header("Sec-WebSocket-Protocol");
        if (ws_protocol_head != null)
            id = Long.valueOf(ctx.queryParam("id"));
        if (id != null) {
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
        final byte[] salt = new byte[KEY_LENGTH];
        RAND.nextBytes(salt);
        return Base64.getEncoder().encodeToString(salt);
    }

    public static String hashPassword(final String password, final String salt) {
        final char[] chars = password.toCharArray();
        final byte[] bytes = salt.getBytes();

        final PBEKeySpec spec = new PBEKeySpec(chars, bytes, ITERATIONS, KEY_LENGTH);

        Arrays.fill(chars, Character.MIN_VALUE);

        try {
            final SecretKeyFactory fac = SecretKeyFactory.getInstance(ALGORITHM);
            final byte[] securePassword = fac.generateSecret(spec).getEncoded();
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
