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

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;

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
import it.cnr.istc.pst.exploraa.server.db.LessonEntity;
import it.cnr.istc.pst.exploraa.server.db.UserEntity;;

/**
 * ExPLoRAA Server
 */
public class App {

    static final Logger LOG = LoggerFactory.getLogger(App.class);
    static final EntityManagerFactory EMF = Persistence.createEntityManagerFactory("ExPLoRAA_PU");

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
                sslConnector.setPort(443);
                final ServerConnector connector = new ServerConnector(server);
                connector.setPort(80);
                server.setConnectors(new Connector[] { sslConnector, connector });
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
            });
        });

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
            });
        });

        // we start the app..
        app.start(7000);

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
                        long user_id = Long.parseLong(idm.getClientID());
                        UserController.ONLINE.remove(user_id);

                        // we broadcast the information that the user is no more online..
                        mqtt_broker.internalPublish(MqttMessageBuilders.publish().topicName(user_id + "/output/on-line")
                                .retained(true).qos(MqttQoS.EXACTLY_ONCE)
                                .payload(Unpooled.copiedBuffer(Boolean.FALSE.toString().getBytes(UTF_8))).build(),
                                getID());
                    }

                    @Override
                    public void onConnectionLost(final InterceptConnectionLostMessage iclm) {
                        long user_id = Long.parseLong(iclm.getClientID());
                        UserController.ONLINE.remove(user_id);

                        // we broadcast the information that the user is no more online..
                        mqtt_broker.internalPublish(MqttMessageBuilders.publish().topicName(user_id + "/output/on-line")
                                .retained(true).qos(MqttQoS.EXACTLY_ONCE)
                                .payload(Unpooled.copiedBuffer(Boolean.FALSE.toString().getBytes(UTF_8))).build(),
                                getID());
                    }

                    @Override
                    public void onConnect(final InterceptConnectMessage icm) {
                        long user_id = Long.parseLong(icm.getClientID());
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
        // determine user role based on request
        // typically done by inspecting headers
        return ExplRole.Admin;
    }

    enum ExplRole implements Role {
        Guest, User, Admin
    }
}