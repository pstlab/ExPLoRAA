package it.cnr.istc.pst.exploraa.server;

import static java.nio.charset.StandardCharsets.UTF_8;

import java.io.IOException;
import java.util.Collections;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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

public class MessagingService {

    static final Logger LOG = LoggerFactory.getLogger(MessagingService.class);
    private static MessagingService instance;
    private final Server mqtt_broker;

    /**
     * @return the instance
     */
    public static MessagingService getInstance() {
        if (instance == null)
            instance = new MessagingService();
        return instance;
    }

    private MessagingService() {
        // we create the MQTT broker..
        mqtt_broker = new Server();
    }

    public void start() {
        // we start the MQTT broker..
        try {
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
                            mqtt_broker.internalPublish(MqttMessageBuilders.publish()
                                    .topicName(user_id + "/output/on-line").retained(true).qos(MqttQoS.EXACTLY_ONCE)
                                    .payload(Unpooled.copiedBuffer(Boolean.FALSE.toString().getBytes(UTF_8))).build(),
                                    getID());
                        }

                        @Override
                        public void onConnectionLost(final InterceptConnectionLostMessage iclm) {
                            long user_id = Long.parseLong(iclm.getClientID().replace("user-", ""));
                            UserController.ONLINE.remove(user_id);

                            // we broadcast the information that the user is no more online..
                            mqtt_broker.internalPublish(MqttMessageBuilders.publish()
                                    .topicName(user_id + "/output/on-line").retained(true).qos(MqttQoS.EXACTLY_ONCE)
                                    .payload(Unpooled.copiedBuffer(Boolean.FALSE.toString().getBytes(UTF_8))).build(),
                                    getID());
                        }

                        @Override
                        public void onConnect(final InterceptConnectMessage icm) {
                            long user_id = Long.parseLong(icm.getClientID().replace("user-", ""));
                            UserController.ONLINE.add(user_id);

                            // we broadcast the information that the user is currently online..
                            mqtt_broker.internalPublish(MqttMessageBuilders.publish()
                                    .topicName(user_id + "/output/on-line").retained(true).qos(MqttQoS.EXACTLY_ONCE)
                                    .payload(Unpooled.copiedBuffer(Boolean.TRUE.toString().getBytes(UTF_8))).build(),
                                    getID());
                        }

                        @Override
                        public void onPublish(final InterceptPublishMessage msg) {
                            final String decodedPayload = new String(ByteBufUtil.getBytes(msg.getPayload()), UTF_8);
                            LOG.info(
                                    "Received message on topic: " + msg.getTopicName() + " content: " + decodedPayload);
                        }
                    }));
        } catch (IOException e) {
            LOG.error("Failed at starting messaging service..", e);
        }
    }

    public void stop() {
        mqtt_broker.stopServer();
    }
}