package it.cnr.istc.pst.exploraa.server;

import java.util.Collections;
import java.util.List;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;

import io.javalin.Javalin;
import static java.nio.charset.StandardCharsets.UTF_8;

import java.io.IOException;

import io.moquette.broker.Server;
import io.moquette.broker.config.ClasspathResourceLoader;
import io.moquette.broker.config.ResourceLoaderConfig;
import io.moquette.interception.AbstractInterceptHandler;
import io.moquette.interception.messages.InterceptAcknowledgedMessage;
import io.moquette.interception.messages.InterceptConnectMessage;
import io.moquette.interception.messages.InterceptConnectionLostMessage;
import io.moquette.interception.messages.InterceptDisconnectMessage;
import io.moquette.interception.messages.InterceptPublishMessage;
import it.cnr.istc.pst.exploraa.server.db.LessonEntity;

/**
 * ExPLoRAA Server
 */
public class App {

    static final EntityManagerFactory EMF = Persistence.createEntityManagerFactory("ExPLoRAA_PU");

    public static void main(String[] args) throws IOException {
        // we create the app..
        final Javalin app = Javalin.create();

        app.events(event -> {
            event.serverStarting(() -> {
                System.out.println("Starting ExPLoRAA server..");
            });
            event.serverStarted(() -> {
                System.out.println("ExPLoRAA server is running..");
                EntityManager em = EMF.createEntityManager();
                List<LessonEntity> lessons = em.createQuery("SELECT le FROM LessonEntity le", LessonEntity.class)
                        .getResultList();

                System.out.println("Loading " + lessons.size() + " lessons..");
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
                        System.out.println(idm);
                    }

                    @Override
                    public void onConnectionLost(InterceptConnectionLostMessage iclm) {
                        System.out.println(iclm);
                    }

                    @Override
                    public void onConnect(InterceptConnectMessage icm) {
                        System.out.println(icm);
                    }

                    @Override
                    public void onMessageAcknowledged(InterceptAcknowledgedMessage msg) {
                        System.out.println(msg);
                    }

                    @Override
                    public void onPublish(InterceptPublishMessage msg) {
                        final String decodedPayload = new String(msg.getPayload().array(), UTF_8);
                        System.out.println("Received on topic: " + msg.getTopicName() + " content: " + decodedPayload);
                    }
                }));

        Runtime.getRuntime().addShutdownHook(new Thread(() -> {
            app.stop();
            mqtt_broker.stopServer();
        }));
    }
}