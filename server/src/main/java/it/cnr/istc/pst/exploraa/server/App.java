package it.cnr.istc.pst.exploraa.server;

import java.util.Collections;

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

/**
 * ExPLoRAA Server
 */
public class App {

    public static void main(String[] args) throws IOException {
        // we create the app..
        final Javalin app = Javalin.create();

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