package it.cnr.istc.exploraa;

import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.activemq.broker.Broker;
import org.apache.activemq.broker.BrokerFilter;
import org.apache.activemq.broker.BrokerPlugin;
import org.apache.activemq.broker.BrokerService;
import org.apache.activemq.broker.ConnectionContext;
import org.apache.activemq.broker.TransportConnector;
import org.apache.activemq.command.ConnectionInfo;

/**
 * This is the main server class.
 */
public class App {

    private static final Logger LOG = Logger.getLogger(App.class.getName());

    public static void main(String[] args) {
        final Properties properties = new Properties();
        ClassLoader loader = Thread.currentThread().getContextClassLoader();
        try (InputStream resourceStream = loader.getResourceAsStream("config.properties")) {
            properties.load(resourceStream);
        } catch (IOException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }

        // we start the MQTT broker..
        BrokerService broker = new BrokerService();
        broker.setPersistent(false);
        TransportConnector connector;
        try {
            connector = broker
                    .addConnector("mqtt://" + properties.getProperty("host") + ":" + properties.getProperty("port"));
            connector.setAllowLinkStealing(true);
            broker.setPlugins(new BrokerPlugin[] { new BrokerPlugin() {
                @Override
                public Broker installPlugin(Broker broker) throws Exception {
                    return new BrokerFilter(broker) {
                        @Override
                        public void addConnection(ConnectionContext context, ConnectionInfo info) throws Exception {
                            LOG.log(Level.INFO, "New connection: {0}", info.getClientId());
                            super.addConnection(context, info);
                        }

                        @Override
                        public void removeConnection(ConnectionContext context, ConnectionInfo info, Throwable error)
                                throws Exception {
                            LOG.log(Level.INFO, "Lost connection: {0}", info.getClientId());
                            super.removeConnection(context, info, error);
                        }
                    };
                }
            } });

            LOG.info("Starting MQTT Broker..");
            broker.start();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
