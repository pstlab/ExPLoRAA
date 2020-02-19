package it.cnr.istc.pst.exploraa.server;

import static io.javalin.apibuilder.ApiBuilder.delete;
import static io.javalin.apibuilder.ApiBuilder.get;
import static io.javalin.apibuilder.ApiBuilder.patch;
import static io.javalin.apibuilder.ApiBuilder.path;
import static io.javalin.apibuilder.ApiBuilder.post;
import static io.javalin.core.security.SecurityUtil.roles;
import static java.nio.charset.StandardCharsets.UTF_8;

import java.io.IOException;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;

import com.fasterxml.jackson.databind.ObjectMapper;

import org.eclipse.jetty.server.Connector;
import org.eclipse.jetty.server.ServerConnector;
import org.eclipse.jetty.util.ssl.SslContextFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.javalin.Javalin;
import io.javalin.core.security.Role;
import io.javalin.http.Context;
import io.javalin.http.Handler;
import io.javalin.http.UnauthorizedResponse;
import io.moquette.broker.Server;
import io.moquette.broker.config.ClasspathResourceLoader;
import io.moquette.broker.config.ResourceLoaderConfig;
import io.moquette.interception.AbstractInterceptHandler;
import io.moquette.interception.messages.InterceptConnectMessage;
import io.moquette.interception.messages.InterceptConnectionLostMessage;
import io.moquette.interception.messages.InterceptDisconnectMessage;
import io.moquette.interception.messages.InterceptPublishMessage;
import io.netty.buffer.ByteBufUtil;
import io.netty.buffer.Unpooled;
import io.netty.handler.codec.mqtt.MqttMessageBuilders;
import io.netty.handler.codec.mqtt.MqttQoS;
import it.cnr.istc.pst.exploraa.api.Lesson;
import it.cnr.istc.pst.exploraa.api.LessonModel;
import it.cnr.istc.pst.exploraa.server.db.LessonEntity;
import it.cnr.istc.pst.exploraa.server.db.UserEntity;
import it.cnr.istc.pst.exploraa.server.solver.LessonManager;;

/**
 * ExPLoRAA Server
 */
public class App {

    static final Logger LOG = LoggerFactory.getLogger(App.class);
    static final EntityManagerFactory EMF = Persistence.createEntityManagerFactory("ExPLoRAA_PU");
    static final ObjectMapper mapper = new ObjectMapper();

    public static void main(final String[] args) throws IOException {
        // we create the app..
        final Javalin app = Javalin.create(config -> {
            config.enforceSsl = true;
            config.addStaticFiles("/public");
            config.accessManager((final Handler handler, final Context ctx, final Set<Role> permittedRoles) -> {
                if (permittedRoles.contains(getRole(ctx)))
                    handler.handle(ctx);
                else
                    throw new UnauthorizedResponse();
            });
            config.server(() -> {
                final org.eclipse.jetty.server.Server server = new org.eclipse.jetty.server.Server();

                final SslContextFactory sslContextFactory = new SslContextFactory.Server();
                sslContextFactory.setKeyStorePath(App.class.getResource("/keystore.jks").toExternalForm());
                sslContextFactory.setKeyStorePassword("ExPLoRAA001");

                final ServerConnector sslConnector = new ServerConnector(server, sslContextFactory);
                sslConnector.setPort(8181);
                server.setConnectors(new Connector[] { sslConnector });
                return server;
            });
        });

        app.events(event -> {
            event.serverStarting(() -> LOG.info("Starting ExPLoRAA server.."));
            event.serverStarted(() -> {
                LOG.info("ExPLoRAA server is running..");
                final EntityManager em = EMF.createEntityManager();

                final List<UserEntity> users = em.createQuery("SELECT ue FROM UserEntity ue", UserEntity.class)
                        .getResultList();

                LOG.info("Loading {} users..", users.size());

                final List<LessonEntity> lessons = em.createQuery("SELECT le FROM LessonEntity le", LessonEntity.class)
                        .getResultList();

                LOG.info("Loading {} lessons..", lessons.size());
                for (LessonEntity l_entity : lessons) {
                    // warning! we do not store the current time of the lesson, nor its state
                    // if the service is restarted, the lesson is not lost, yet its state is!
                    LessonModel lm = mapper.readValue(l_entity.getModel().getModel(), LessonModel.class);

                    Lesson l = new Lesson(
                            l_entity.getId(), l_entity.getName(), lm.getId(), LessonController.getTopics(lm),
                            LessonController.toTeaching(l_entity.getTeacher()), l_entity.getStudents().stream()
                                    .map(student -> LessonController.toFollowing(student)).collect(Collectors
                                            .toMap(following -> following.getUser().getId(), following -> following)),
                            Lesson.LessonState.Stopped, 0);

                    LessonController.LESSONS.put(l.getId(), new LessonManager(l));
                }
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
                    patch(UserController::updateUser, roles(ExplRole.Admin, ExplRole.User));
                    delete(UserController::deleteUser, roles(ExplRole.Admin, ExplRole.User));
                });
            });
            path("lessons", () -> {
                get(LessonController::getAllLessons, roles(ExplRole.Admin, ExplRole.User));
                post(LessonController::createLesson, roles(ExplRole.Admin, ExplRole.User));
                path(":id", () -> {
                    get(LessonController::getLesson, roles(ExplRole.Admin, ExplRole.User));
                    delete(LessonController::deleteLesson, roles(ExplRole.Admin, ExplRole.User));
                });
                path("play/:id", () -> post(LessonController::playLesson, roles(ExplRole.Admin, ExplRole.User)));
                path("pause/:id", () -> post(LessonController::pauseLesson, roles(ExplRole.Admin, ExplRole.User)));
                path("stop/:id", () -> post(LessonController::stopLesson, roles(ExplRole.Admin, ExplRole.User)));
                path("follow", () -> post(LessonController::followLesson, roles(ExplRole.Admin, ExplRole.User)));
                path("unfollow", () -> post(LessonController::unfollowLesson, roles(ExplRole.Admin, ExplRole.User)));
            });
        });

        // we start the app..
        app.start();

        // we create the MQTT broker..
        final Server mqtt_broker = new Server();

        // we start the MQTT broker..
        mqtt_broker.startServer(new ResourceLoaderConfig(new ClasspathResourceLoader()),
                Collections.singletonList(new AbstractInterceptHandler() {

                    @Override
                    public String getID() {
                        return "EmbeddedLauncherPublishListener";
                    }

                    @Override
                    public void onDisconnect(final InterceptDisconnectMessage idm) {
                        long user_id = Long.parseLong(idm.getClientID().replace("user-", ""));
                        UserController.ONLINE.remove(user_id);

                        // we broadcast the information that the user is no more online..
                        mqtt_broker.internalPublish(MqttMessageBuilders.publish().topicName(user_id + "/output/on-line")
                                .retained(true).qos(MqttQoS.EXACTLY_ONCE)
                                .payload(Unpooled.copiedBuffer(Boolean.FALSE.toString().getBytes(UTF_8))).build(),
                                getID());
                    }

                    @Override
                    public void onConnectionLost(final InterceptConnectionLostMessage iclm) {
                        long user_id = Long.parseLong(iclm.getClientID().replace("user-", ""));
                        UserController.ONLINE.remove(user_id);

                        // we broadcast the information that the user is no more online..
                        mqtt_broker.internalPublish(MqttMessageBuilders.publish().topicName(user_id + "/output/on-line")
                                .retained(true).qos(MqttQoS.EXACTLY_ONCE)
                                .payload(Unpooled.copiedBuffer(Boolean.FALSE.toString().getBytes(UTF_8))).build(),
                                getID());
                    }

                    @Override
                    public void onConnect(final InterceptConnectMessage icm) {
                        long user_id = Long.parseLong(icm.getClientID().replace("user-", ""));
                        UserController.ONLINE.add(user_id);

                        // we broadcast the information that the user is currently online..
                        mqtt_broker.internalPublish(MqttMessageBuilders.publish().topicName(user_id + "/output/on-line")
                                .retained(true).qos(MqttQoS.EXACTLY_ONCE)
                                .payload(Unpooled.copiedBuffer(Boolean.TRUE.toString().getBytes(UTF_8))).build(),
                                getID());
                    }

                    @Override
                    public void onPublish(final InterceptPublishMessage msg) {
                        final String decodedPayload = new String(ByteBufUtil.getBytes(msg.getPayload()), UTF_8);
                        LOG.info("Received on topic: " + msg.getTopicName() + " content: " + decodedPayload);
                    }
                }));

        Runtime.getRuntime().addShutdownHook(new Thread(() -> {
            app.stop();
            mqtt_broker.stopServer();
            EMF.close();
        }));
    }

    static Role getRole(final Context ctx) {
        String auth_head = ctx.header("Authorization");
        if (auth_head == null)
            return ExplRole.Guest;
        EntityManager em = App.EMF.createEntityManager();
        UserEntity user_entity = em.find(UserEntity.class, Long.parseLong(auth_head.replace("Basic ", "")));
        if (user_entity == null)
            return ExplRole.Guest;
        else if (user_entity.getRoles().contains(ExplRole.Admin.name()))
            return ExplRole.Admin;
        else
            return ExplRole.User;
    }

    enum ExplRole implements Role {
        Guest, User, Admin
    }
}