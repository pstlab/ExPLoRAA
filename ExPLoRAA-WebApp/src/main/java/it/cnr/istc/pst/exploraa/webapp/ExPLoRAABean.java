/*
 * Copyright (C) 2018 Riccardo De Benedictis
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package it.cnr.istc.pst.exploraa.webapp;

import it.cnr.istc.pst.exploraa.api.Message;
import it.cnr.istc.pst.exploraa.api.Message.LostParameter;
import it.cnr.istc.pst.exploraa.api.Message.NewParameter;
import it.cnr.istc.pst.exploraa.api.Parameter;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;
import javax.annotation.Resource;
import javax.ejb.ConcurrencyManagement;
import javax.ejb.ConcurrencyManagementType;
import javax.ejb.Lock;
import javax.ejb.LockType;
import javax.ejb.Singleton;
import javax.ejb.Startup;
import javax.json.bind.Jsonb;
import javax.json.bind.JsonbBuilder;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.ws.rs.core.UriBuilder;
import org.apache.activemq.broker.Broker;
import org.apache.activemq.broker.BrokerFilter;
import org.apache.activemq.broker.BrokerPlugin;
import org.apache.activemq.broker.BrokerService;
import org.apache.activemq.broker.ConnectionContext;
import org.apache.activemq.broker.TransportConnector;
import org.apache.activemq.command.ConnectionInfo;
import org.eclipse.paho.client.mqttv3.IMqttDeliveryToken;
import org.eclipse.paho.client.mqttv3.MqttCallback;
import org.eclipse.paho.client.mqttv3.MqttClient;
import org.eclipse.paho.client.mqttv3.MqttConnectOptions;
import org.eclipse.paho.client.mqttv3.MqttException;
import org.eclipse.paho.client.mqttv3.MqttMessage;
import org.eclipse.paho.client.mqttv3.persist.MemoryPersistence;

/**
 *
 * @author Riccardo De Benedictis
 */
@Singleton
@Startup
@ConcurrencyManagement(ConcurrencyManagementType.CONTAINER)
public class ExPLoRAABean {

    private static final Logger LOG = Logger.getLogger(ExPLoRAABean.class.getName());
    public static final Jsonb JSONB = JsonbBuilder.create();
    @Resource(name = "java:app/config")
    private Properties properties;
    private BrokerService broker;
    private MqttClient mqtt;
    /**
     * For each user id, a boolean indicating whether the user is online.
     */
    private final Map<Long, Boolean> online = new HashMap<>();
    /**
     * For each user id, a map of parameter types containing the name of the
     * parameter as key.
     */
    private final Map<Long, Map<String, Parameter>> parameter_types = new HashMap<>();
    /**
     * For each user id, a map of parameter values containing the name of the
     * parameter as key. Notice that parameter values are represented through a
     * map.
     */
    private final Map<Long, Map<String, Map<String, String>>> parameter_values = new HashMap<>();
    /**
     * For each lesson, the context of the lesson.
     */
    private final Map<Long, LessonManager> lessons = new HashMap<>();
    @PersistenceContext
    private EntityManager em;
    private AtomicBoolean busy = new AtomicBoolean(false);

    @PostConstruct
    private void startup() {
        LOG.info("Starting LECTurE Server");

//        // we init the current state..
//        for (UserEntity ue : em.createQuery("SELECT u FROM UserEntity u", UserEntity.class).getResultList()) {
//            newUser(ue.getId());
//        }
        // we start the MQTT broker..
        broker = new BrokerService();
        broker.setPersistent(false);
        try {
            TransportConnector connector = broker.addConnector(UriBuilder.fromUri("mqtt://" + properties.getProperty("mqtt-host") + ":" + properties.getProperty("mqtt-port")).build());
            connector.setAllowLinkStealing(true);
            broker.setPlugins(new BrokerPlugin[]{new BrokerPlugin() {
                @Override
                public Broker installPlugin(Broker broker) throws Exception {
                    return new BrokerFilter(broker) {
                        @Override
                        public void addConnection(ConnectionContext context, ConnectionInfo info) throws Exception {
                            LOG.log(Level.INFO, "New connection: {0}", info.getClientId());
                            if (!info.getClientId().equals(properties.getProperty("mqtt-server-id"))) {
                                long user_id = Long.parseLong(info.getClientId());
                                setOnline(user_id, true);
                                mqtt.publish(user_id + "/output/on-line", Boolean.TRUE.toString().getBytes(), 1, true);
                                mqtt.subscribe(user_id + "/output", (String topic, MqttMessage message) -> {
                                    LOG.log(Level.INFO, "Message arrived: {0} {1}", new Object[]{topic, message});
                                    Message m = JSONB.fromJson(new String(message.getPayload()), Message.class);
                                    switch (m.message_type) {
                                        case NewParameter:
                                            NewParameter new_parameter = JSONB.fromJson(new String(message.getPayload()), NewParameter.class);
                                            newParameter(user_id, new_parameter.parameter);
                                            break;
                                        case LostParameter:
                                            LostParameter lost_parameter = JSONB.fromJson(new String(message.getPayload()), LostParameter.class);
                                            removeParameter(user_id, getParType(user_id, lost_parameter.parameter.name));
                                            break;
//                                        case NewStudent:
//                                            break;
//                                        case Answer:
//                                            break;
                                        default:
                                            throw new AssertionError(m.message_type.name());
                                    }
                                });
                            }
                            super.addConnection(context, info);
                        }

                        @Override
                        public void removeConnection(ConnectionContext context, ConnectionInfo info, Throwable error) throws Exception {
                            LOG.log(Level.INFO, "Lost connection: {0}", info.getClientId());
                            if (!info.getClientId().equals(properties.getProperty("mqtt-server-id"))) {
                                long user_id = Long.parseLong(info.getClientId());
                                setOnline(user_id, false);
                                mqtt.unsubscribe(user_id + "/output");
                                mqtt.publish(user_id + "/output/on-line", Boolean.FALSE.toString().getBytes(), 1, true);
                            }
                            super.removeConnection(context, info, error);
                        }
                    };
                }
            }});
            LOG.info("Starting MQTT Broker");
            broker.start();

            // we connect an MQTT client..
            mqtt = new MqttClient("tcp://" + properties.getProperty("mqtt-host") + ":" + properties.getProperty("mqtt-port"), properties.getProperty("mqtt-server-id"), new MemoryPersistence());
            mqtt.setCallback(new MqttCallback() {
                @Override
                public void connectionLost(Throwable cause) {
                    LOG.log(Level.SEVERE, null, cause);
                }

                @Override
                public void messageArrived(String topic, MqttMessage message) throws Exception {
                    LOG.log(Level.WARNING, "Message arrived: {0} {1}", new Object[]{topic, message});
                }

                @Override
                public void deliveryComplete(IMqttDeliveryToken token) {
                }
            });
            MqttConnectOptions options = new MqttConnectOptions();
            options.setCleanSession(false);
            options.setAutomaticReconnect(true);
            LOG.info("Connecting MQTT Client");
            mqtt.connect(options);
        } catch (Exception ex) {
            LOG.log(Level.SEVERE, null, ex);
        }

//        // we create a solver for each of the already created lessons..
//        List<LessonEntity> c_lessons = em.createQuery("SELECT l FROM LessonEntity l", LessonEntity.class).getResultList();
//        for (LessonEntity l_entity : c_lessons) {
//            // warning! we do not store the current time of the lesson, nor its state.. if the service is restarted, the lesson is not lost, yet its state is!
//            Lesson l = new Lesson(l_entity.getId(), l_entity.getTeacher().getId(), l_entity.getName(), Lesson.LessonState.Stopped, 0, l_entity.getModel().getId(), l_entity.getRoles().stream().collect(Collectors.toMap(r -> r.getName(), r -> r.getStudent().getId())), Collections.emptyList(), Collections.emptyList());
//            LessonModel lm = JSONB.fromJson(l_entity.getModel().getModel(), LessonModel.class);
//            newLesson(l, lm);
//            solveLesson(l.id);
//        }
    }

    @PreDestroy
    private void shutdown() {
        LOG.info("Stopping LECTurE MQTT Broker");
        try {
            LOG.info("Disconnecting MQTT Client");
            mqtt.disconnect();
            mqtt.close();
            LOG.info("Stopping  MQTT Broker");
            broker.stop();
            online.clear();
            parameter_types.clear();
            parameter_values.clear();
        } catch (Exception ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
    }

    @Lock(LockType.WRITE)
    public void setOnline(long user_id, boolean online) {
        this.online.put(user_id, online);
    }

    @Lock(LockType.READ)
    public boolean isOnline(long user_id) {
        return online.get(user_id);
    }

    @Lock(LockType.READ)
    public Map<String, Parameter> getParTypes(long user_id) {
        return Collections.unmodifiableMap(parameter_types.get(user_id));
    }

    @Lock(LockType.READ)
    public Parameter getParType(long user_id, String par_name) {
        return parameter_types.get(user_id).get(par_name);
    }

    @Lock(LockType.READ)
    public Map<String, Map<String, String>> getParValues(long user_id) {
        return Collections.unmodifiableMap(parameter_values.get(user_id));
    }

    @Lock(LockType.WRITE)
    public void newUser(long user_id) {
        online.put(user_id, Boolean.FALSE);
        parameter_types.put(user_id, new HashMap<>());
        parameter_values.put(user_id, new HashMap<>());
    }

    @Lock(LockType.WRITE)
    public void deleteUser(long user_id) {
        parameter_types.remove(user_id);
        parameter_values.remove(user_id);
    }

    @Lock(LockType.WRITE)
    public void newParameter(long user_id, Parameter par) {
        parameter_types.get(user_id).put(par.name, par);
        try {
            mqtt.subscribe(user_id + "/output/" + par.name, (String topic, MqttMessage message) -> {
                Map<String, String> par_vals = JSONB.fromJson(new String(message.getPayload()), new HashMap<String, String>() {
                }.getClass().getGenericSuperclass());
                newParameterValue(user_id, par.name, par_vals);
            });
        } catch (MqttException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
    }

    @Lock(LockType.WRITE)
    public void removeParameter(long user_id, Parameter par) {
        parameter_types.get(user_id).remove(par.name);
        try {
            mqtt.unsubscribe(user_id + "/output/" + par.name);
        } catch (MqttException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
    }

    @Lock(LockType.WRITE)
    public void newParameterValue(long user_id, String par, Map<String, String> val) {
        parameter_values.get(user_id).put(par, val);
    }
}
