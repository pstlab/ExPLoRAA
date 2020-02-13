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
import java.util.HashSet;
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
import it.cnr.istc.pst.exploraa.server.db.LessonEntity;
import it.cnr.istc.pst.exploraa.server.db.UserEntity;;

/**
 * ExPLoRAA Server
 */
public class App {

    static final Logger LOG = LoggerFactory.getLogger(App.class);
    static final EntityManagerFactory EMF = Persistence.createEntityManagerFactory("ExPLoRAA_PU");
    private static final Set<String> ON_LINE = new HashSet<>();

    public static void main(String[] args) throws IOException {
        // we create the app..
        final Javalin app = Javalin.create(config -> {
            config.enforceSsl = true;
            config.addStaticFiles("/public");
            config.accessManager((Handler handler, Context ctx, Set<Role> permittedRoles) -> {
                if (permittedRoles.contains(getRole(ctx)))
                    handler.handle(ctx);
                else
                    throw new UnauthorizedResponse();
            });
            config.server(() -> {
                org.eclipse.jetty.server.Server server = new org.eclipse.jetty.server.Server();

                SslContextFactory sslContextFactory = new SslContextFactory.Server();
                sslContextFactory.setKeyStorePath(App.class.getResource("/keystore.jks").toExternalForm());
                sslContextFactory.setKeyStorePassword("ExPLoRAA001");

                ServerConnector sslConnector = new ServerConnector(server, sslContextFactory);
                sslConnector.setPort(443);
                ServerConnector connector = new ServerConnector(server);
                connector.setPort(80);
                server.setConnectors(new Connector[] { sslConnector, connector });
                return server;
            });
        });

        app.events(event -> {
            event.serverStarting(() -> LOG.info("Starting ExPLoRAA server.."));
            event.serverStarted(() -> {
                LOG.info("ExPLoRAA server is running..");
                EntityManager em = EMF.createEntityManager();

                List<UserEntity> users = em.createQuery("SELECT ue FROM UserEntity ue", UserEntity.class)
                        .getResultList();

                LOG.info("Loading {} users..", users.size());
                for (UserEntity ue : users)
                    UserController.ONLINE.put(ue.getId(), false);

                List<LessonEntity> lessons = em.createQuery("SELECT le FROM LessonEntity le", LessonEntity.class)
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
                    public void onDisconnect(InterceptDisconnectMessage idm) {
                        ON_LINE.remove(idm.getClientID());
                    }

                    @Override
                    public void onConnectionLost(InterceptConnectionLostMessage iclm) {
                        ON_LINE.remove(iclm.getClientID());
                    }

                    @Override
                    public void onConnect(InterceptConnectMessage icm) {
                        ON_LINE.add(icm.getClientID());
                    }

                    @Override
                    public void onPublish(InterceptPublishMessage msg) {
                        final String decodedPayload = new String(ByteBufUtil.getBytes(msg.getPayload()), UTF_8);
                        System.out.println("Received on topic: " + msg.getTopicName() + " content: " + decodedPayload);
                    }
                }));

        Runtime.getRuntime().addShutdownHook(new Thread(() -> {
            app.stop();
            mqtt_broker.stopServer();
            EMF.close();
        }));
    }

    static Role getRole(Context ctx) {
        // determine user role based on request
        // typically done by inspecting headers
        return ExplRole.Admin;
    }

    enum ExplRole implements Role {
        Guest, User, Admin
    }
}