package it.cnr.istc.pst.exploraa.server;

import static io.javalin.apibuilder.ApiBuilder.delete;
import static io.javalin.apibuilder.ApiBuilder.get;
import static io.javalin.apibuilder.ApiBuilder.patch;
import static io.javalin.apibuilder.ApiBuilder.path;
import static io.javalin.apibuilder.ApiBuilder.post;
import static io.javalin.core.security.SecurityUtil.roles;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import javax.persistence.EntityManager;

import org.eclipse.jetty.alpn.server.ALPNServerConnectionFactory;
import org.eclipse.jetty.http2.HTTP2Cipher;
import org.eclipse.jetty.http2.server.HTTP2ServerConnectionFactory;
import org.eclipse.jetty.server.HttpConfiguration;
import org.eclipse.jetty.server.HttpConnectionFactory;
import org.eclipse.jetty.server.SecureRequestCustomizer;
import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.server.ServerConnector;
import org.eclipse.jetty.server.SslConnectionFactory;
import org.eclipse.jetty.util.ssl.SslContextFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.javalin.Javalin;
import io.javalin.core.security.Role;
import io.javalin.http.Context;
import io.javalin.http.Handler;
import io.javalin.http.UnauthorizedResponse;
import it.cnr.istc.pst.exploraa.api.Lesson;
import it.cnr.istc.pst.exploraa.api.LessonModel;
import it.cnr.istc.pst.exploraa.server.db.LessonEntity;
import it.cnr.istc.pst.exploraa.server.db.UserEntity;
import it.cnr.istc.pst.exploraa.server.solver.LessonManager;

public class RESTService {

    static final Logger LOG = LoggerFactory.getLogger(RESTService.class);
    private static RESTService instance;
    private final Javalin app;

    /**
     * @return the instance
     */
    public static RESTService getInstance() {
        if (instance == null)
            instance = new RESTService();
        return instance;
    }

    private RESTService() {
        // we create the app..
        app = Javalin.create(config -> {
            config.server(RESTService::createHttp2Server);
            config.addStaticFiles("/public");
            config.accessManager((final Handler handler, final Context ctx, final Set<Role> permittedRoles) -> {
                if (permittedRoles.contains(getRole(ctx)))
                    handler.handle(ctx);
                else
                    throw new UnauthorizedResponse();
            });
            // config.enableCorsForAllOrigins();
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
                for (LessonEntity l_entity : lessons) {
                    // warning! we do not store the current time of the lesson, nor its state
                    // if the service is restarted, the lesson is not lost, yet its state is!
                    LessonModel lm = App.MAPPER.readValue(l_entity.getModel().getModel(), LessonModel.class);

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
    }

    public void start() {
        // we start the app..
        app.start();
    }

    public void stop() {
        // we stop the app..
        app.stop();
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

    private static Server createHttp2Server() {
        final Server server = new Server();

        // HTTP Configuration
        final HttpConfiguration httpConfig = new HttpConfiguration();
        httpConfig.setSendServerVersion(false);
        httpConfig.setSecureScheme("https");
        httpConfig.setSecurePort(8443);

        // SSL Context Factory for HTTPS and HTTP/2
        final SslContextFactory sslContextFactory = new SslContextFactory.Server();
        sslContextFactory.setKeyStorePath(App.class.getResource("/keystore.jks").toExternalForm());
        sslContextFactory.setKeyStorePassword("ExPLoRAA123");
        sslContextFactory.setCipherComparator(HTTP2Cipher.COMPARATOR);
        sslContextFactory.setProvider("Conscrypt");

        // HTTPS Configuration
        final HttpConfiguration httpsConfig = new HttpConfiguration(httpConfig);
        httpsConfig.addCustomizer(new SecureRequestCustomizer());

        // HTTP/2 Connection Factory
        final HTTP2ServerConnectionFactory h2 = new HTTP2ServerConnectionFactory(httpsConfig);
        final ALPNServerConnectionFactory alpn = new ALPNServerConnectionFactory();
        alpn.setDefaultProtocol("h2");

        // SSL Connection Factory
        final SslConnectionFactory ssl = new SslConnectionFactory(sslContextFactory, alpn.getProtocol());

        // HTTP/2 Connector
        final ServerConnector http2Connector = new ServerConnector(server, ssl, alpn, h2,
                new HttpConnectionFactory(httpsConfig));
        http2Connector.setHost("192.168.1.101");
        http2Connector.setPort(8443);
        server.addConnector(http2Connector);

        return server;
    }

    enum ExplRole implements Role {
        Guest, User, Admin
    }
}