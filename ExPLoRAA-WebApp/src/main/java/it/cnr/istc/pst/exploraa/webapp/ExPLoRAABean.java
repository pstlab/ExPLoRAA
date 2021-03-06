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

import it.cnr.istc.pst.exploraa.api.Follow;
import it.cnr.istc.pst.exploraa.api.Lesson;
import it.cnr.istc.pst.exploraa.api.LessonModel;
import it.cnr.istc.pst.exploraa.api.Message;
import it.cnr.istc.pst.exploraa.api.Message.RemoveParameter;
import it.cnr.istc.pst.exploraa.api.Message.NewParameter;
import it.cnr.istc.pst.exploraa.api.Parameter;
import it.cnr.istc.pst.exploraa.api.Teach;
import it.cnr.istc.pst.exploraa.api.User;
import it.cnr.istc.pst.exploraa.webapp.db.FollowEntity;
import it.cnr.istc.pst.exploraa.webapp.db.LessonEntity;
import it.cnr.istc.pst.exploraa.webapp.db.UserEntity;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
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
import javax.ejb.Schedule;
import javax.ejb.Singleton;
import javax.ejb.Startup;
import javax.json.bind.Jsonb;
import javax.json.bind.JsonbBuilder;
import javax.json.bind.JsonbConfig;
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
    public static final Jsonb JSONB = JsonbBuilder.create(new JsonbConfig().withAdapters(Message.ADAPTER, LessonModel.ADAPTER));
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

        // we init the current state..
        for (UserEntity ue : em.createQuery("SELECT u FROM UserEntity u", UserEntity.class).getResultList()) {
            newUser(ue.getId());
        }

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
                                        case RemoveParameter:
                                            RemoveParameter lost_parameter = JSONB.fromJson(new String(message.getPayload()), RemoveParameter.class);
                                            removeParameter(user_id, getParType(user_id, lost_parameter.parameter));
                                            break;
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

        // we create a solver for each of the already created lessons..
        LOG.info("Loading lessons..");
        List<LessonEntity> c_lessons = em.createQuery("SELECT l FROM LessonEntity l", LessonEntity.class).getResultList();
        for (LessonEntity l_entity : c_lessons) {
            // warning! we do not store the current time of the lesson, nor its state.. if the service is restarted, the lesson is not lost, yet its state is!
            LessonModel lm = JSONB.fromJson(l_entity.getModel().getModel(), LessonModel.class);

            Set<String> topics = new HashSet<>();
            for (LessonModel.StimulusTemplate template : lm.stimuli.values()) {
                switch (template.type) {
                    case Text:
                        if (((LessonModel.StimulusTemplate.TextStimulusTemplate) template).topics != null) {
                            topics.addAll(((LessonModel.StimulusTemplate.TextStimulusTemplate) template).topics);
                        }
                        break;
                    case URL:
                        if (((LessonModel.StimulusTemplate.URLStimulusTemplate) template).topics != null) {
                            topics.addAll(((LessonModel.StimulusTemplate.URLStimulusTemplate) template).topics);
                        }
                        break;
                    case Question:
                        if (((LessonModel.StimulusTemplate.QuestionStimulusTemplate) template).topics != null) {
                            topics.addAll(((LessonModel.StimulusTemplate.QuestionStimulusTemplate) template).topics);
                        }
                        break;
                    case Root:
                    case Trigger:
                        break;
                    default:
                        throw new AssertionError(template.type.name());
                }
            }

            Lesson l = new Lesson(l_entity.getId(), l_entity.getName(), lm, topics, new ArrayList<>(), new ArrayList<>(), new Teach(new User(l_entity.getTeachedBy().getTeacher().getId(), l_entity.getTeachedBy().getTeacher().getEmail(), l_entity.getTeachedBy().getTeacher().getFirstName(), l_entity.getTeachedBy().getTeacher().getLastName(), online.get(l_entity.getTeachedBy().getTeacher().getId()), null, null, null, null, null), null), new HashMap<>(), Lesson.LessonState.Stopped, 0);
            for (FollowEntity follow : l_entity.getStudents()) {
                l.students.put(follow.getStudent().getId(), new Follow(new User(follow.getStudent().getId(), follow.getStudent().getEmail(), follow.getStudent().getFirstName(), follow.getStudent().getLastName(), isOnline(follow.getStudent().getId()), getParTypes(follow.getStudent().getId()), getParValues(follow.getStudent().getId()), null, null, null), null, new HashSet<>(follow.getInterests())));
            }

            newLesson(l);
            solveLesson(l.id);
        }
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
        LOG.log(Level.INFO, "New parameter value for user: {0} parameter: {1} value: {2}", new Object[]{user_id, par, val});
        parameter_values.get(user_id).put(par, val);
        for (LessonManager lm : lessons.values()) {
            if (lm.getLesson().students.containsKey(user_id)) {
                for (LessonManager.SolverToken tk : lm.getTriggerableTokens()) {
                    if (involves(((LessonModel.StimulusTemplate.TriggerTemplate) tk.template).condition, par) && isSatisfied(((LessonModel.StimulusTemplate.TriggerTemplate) tk.template).condition, parameter_values.get(user_id)) && lm.isTriggerableBy(tk, user_id)) {
                        // the token 'tk' should be triggered..
                        lm.trigger(tk, user_id);
                    }
                }
            }
        }
    }

    @Lock(LockType.WRITE)
    public void answerQuestion(long user_id, long lesson_id, int question_id, int answer_id) {
        try {
            // we notify the teacher that a student has answered a question..
            mqtt.publish(lessons.get(lesson_id).getLesson().teacher.user.id + "/input", JSONB.toJson(new Message.Stimulus.QuestionStimulus.Answer(lesson_id, question_id, answer_id)).getBytes(), 1, false);
            // we compute the answer's consequences..
            lessons.get(lesson_id).answerQuestion(user_id, question_id, answer_id);
        } catch (MqttException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
    }

    @Lock(LockType.WRITE)
    public void newLesson(Lesson lesson) {
        LessonManager manager = new LessonManager(lesson);
        lessons.put(lesson.id, manager);
        manager.addSolverListener(new LessonManagerListener() {
            @Override
            public void newToken(LessonManager.SolverToken tk) {
                Double lb = manager.network.lb(tk.tp);
                Double ub = manager.network.ub(tk.tp);
                Message.Token token = new Message.Token(lesson.id, tk.tp, tk.cause != null ? tk.cause.tp : null, lb != Double.NEGATIVE_INFINITY ? lb.longValue() : null, ub != Double.POSITIVE_INFINITY ? ub.longValue() : null, (long) manager.network.value(tk.tp), tk.template.name);
                lesson.tokens.add(token);
                try {
                    mqtt.publish(lesson.teacher.user.id + "/input", JSONB.toJson(token).getBytes(), 1, false);
                } catch (MqttException ex) {
                    LOG.log(Level.SEVERE, null, ex);
                }
            }

            @Override
            public void movedToken(LessonManager.SolverToken tk) {
                Message.Token token = lesson.tokens.stream().filter(t -> t.id == tk.tp).findAny().get();
                Double lb = manager.network.lb(tk.tp);
                Double ub = manager.network.ub(tk.tp);
                token.min = lb != Double.NEGATIVE_INFINITY ? lb.longValue() : null;
                token.max = ub != Double.POSITIVE_INFINITY ? ub.longValue() : null;
                token.time = (long) manager.network.value(tk.tp);
                try {
                    mqtt.publish(lesson.teacher.user.id + "/input", JSONB.toJson(new Message.TokenUpdate(lesson.id, tk.tp, token.min, token.max, token.time)).getBytes(), 1, false);
                } catch (MqttException ex) {
                    LOG.log(Level.SEVERE, null, ex);
                }
            }

            @Override
            public void executeToken(LessonManager.SolverToken tk, final LessonManager.TriggerContext ctx) {
                if (tk.template.type != LessonModel.StimulusTemplate.StimulusTemplateType.Root) {
                    // the students both interested and having proper parameter values for this stimulus..
                    Set<Long> students = new HashSet<>();
                    if (ctx == null) {
                        if (tk.template.type == LessonModel.StimulusTemplate.StimulusTemplateType.Trigger) {
                            students.addAll(lesson.students.keySet());
                        } else {
                            for (Follow follow : lesson.students.values()) {
                                for (String interest : follow.interests) {
                                    if (tk.template.execution_condition == null || (isOnline(follow.user.id) && isSatisfied(tk.template.execution_condition, parameter_values.get(follow.user.id)))) {
                                        switch (tk.template.type) {
                                            case Root:
                                            case Trigger:
                                                break;
                                            case Text:
                                                if (((LessonModel.StimulusTemplate.TextStimulusTemplate) tk.template).topics.contains(interest)) {
                                                    students.add(follow.user.id);
                                                }
                                                break;
                                            case URL:
                                                if (((LessonModel.StimulusTemplate.URLStimulusTemplate) tk.template).topics.contains(interest)) {
                                                    students.add(follow.user.id);
                                                }
                                                break;
                                            case Question:
                                                if (((LessonModel.StimulusTemplate.QuestionStimulusTemplate) tk.template).topics.contains(interest)) {
                                                    students.add(follow.user.id);
                                                }
                                                break;
                                            default:
                                                throw new AssertionError(tk.template.type.name());
                                        }
                                    }
                                }
                            }
                        }
                    } else if ((ctx.getSourceToken().template instanceof LessonModel.StimulusTemplate.QuestionStimulusTemplate)) {
                        if (tk.template.execution_condition == null || (isOnline(ctx.getUserId()) && isSatisfied(tk.template.execution_condition, parameter_values.get(ctx.getUserId())))) {
                            switch (tk.template.type) {
                                case Root:
                                case Trigger:
                                    break;
                                case Text:
                                    if (((LessonModel.StimulusTemplate.QuestionStimulusTemplate) ctx.getSourceToken().template).scope == LessonModel.StimulusTemplate.EffectScope.Group) {
                                        students.addAll(lesson.students.keySet());
                                    } else {
                                        students.add(ctx.getUserId());
                                    }
                                    break;
                                case URL:
                                    if (((LessonModel.StimulusTemplate.QuestionStimulusTemplate) ctx.getSourceToken().template).scope == LessonModel.StimulusTemplate.EffectScope.Group) {
                                        students.addAll(lesson.students.keySet());
                                    } else {
                                        students.add(ctx.getUserId());
                                    }
                                    break;
                                case Question:
                                    if (((LessonModel.StimulusTemplate.QuestionStimulusTemplate) ctx.getSourceToken().template).scope == LessonModel.StimulusTemplate.EffectScope.Group) {
                                        students.addAll(lesson.students.keySet());
                                    } else {
                                        students.add(ctx.getUserId());
                                    }
                                    break;
                                default:
                                    throw new AssertionError(tk.template.type.name());
                            }
                        }
                    } else if ((ctx.getSourceToken().template instanceof LessonModel.StimulusTemplate.TriggerTemplate)) {
                        if (tk.template.execution_condition == null || (isOnline(ctx.getUserId()) && isSatisfied(tk.template.execution_condition, parameter_values.get(ctx.getUserId())))) {
                            switch (tk.template.type) {
                                case Root:
                                case Trigger:
                                    break;
                                case Text:
                                    if (((LessonModel.StimulusTemplate.TriggerTemplate) ctx.getSourceToken().template).scope == LessonModel.StimulusTemplate.EffectScope.Self) {
                                        students.add(ctx.getUserId());
                                    } else {
                                        students.addAll(lesson.students.keySet());
                                    }
                                    break;
                                case URL:
                                    if (((LessonModel.StimulusTemplate.TriggerTemplate) ctx.getSourceToken().template).scope == LessonModel.StimulusTemplate.EffectScope.Self) {
                                        students.add(ctx.getUserId());
                                    } else {
                                        students.addAll(lesson.students.keySet());
                                    }
                                    break;
                                case Question:
                                    if (((LessonModel.StimulusTemplate.TriggerTemplate) ctx.getSourceToken().template).scope == LessonModel.StimulusTemplate.EffectScope.Self) {
                                        students.add(ctx.getUserId());
                                    } else {
                                        students.addAll(lesson.students.keySet());
                                    }
                                    break;
                                default:
                                    throw new AssertionError(tk.template.type.name());
                            }
                        }
                    }

                    if (tk.template.type == LessonModel.StimulusTemplate.StimulusTemplateType.Trigger) {
                        // we check if the token can be triggered..
                        for (Long student : students) {
                            if (isOnline(student) && isSatisfied(((LessonModel.StimulusTemplate.TriggerTemplate) tk.template).condition, parameter_values.get(student))) {
                                // we also trigger the token..
                                manager.trigger(tk, student);
                            }
                        }
                    } else {
                        // we execute the token..
                        manager.dispatchToken(tk, students);
                    }
                }
            }

            @Override
            public void executeStimulus(Message.Stimulus s, long user_id) {
                try {
                    mqtt.publish(user_id + "/input", JSONB.toJson(s, Message.class).getBytes(), 1, false);
                } catch (MqttException ex) {
                    LOG.log(Level.SEVERE, null, ex);
                }
            }

            @Override
            public void hideStimulus(Message.Stimulus s, long user_id) {
                try {
                    mqtt.publish(user_id + "/input", JSONB.toJson(new Message.RemoveStimulus(lesson.id, s.id)).getBytes(), 1, false);
                } catch (MqttException ex) {
                    LOG.log(Level.SEVERE, null, ex);
                }
            }

            @Override
            public void removeToken(LessonManager.SolverToken tk) {
                // we remove the token from the lesson..
                lesson.tokens.removeIf(t -> t.id == tk.tp);
                try {
                    // we notify the teacher that a token has to be removed..
                    mqtt.publish(lesson.teacher.user.id + "/input", JSONB.toJson(new Message.RemoveToken(lesson.id, tk.tp)).getBytes(), 1, false);
                } catch (MqttException ex) {
                    LOG.log(Level.SEVERE, null, ex);
                }
            }

            @Override
            public void newTime(long time) {
                try {
                    // we broadcast the new lesson time..
                    mqtt.publish(lesson.teacher.user.id + "/input/lesson-" + lesson.id + "/time", Long.toString(time).getBytes(), 1, false);
                } catch (MqttException ex) {
                    LOG.log(Level.SEVERE, null, ex);
                }
            }
        });
    }

    @Lock(LockType.WRITE)
    public void follow(User student, long lesson, Set<String> interests) {
        LessonManager lm = lessons.get(lesson);
        lm.follow(student, interests);
        Lesson l = lm.getLesson();

        try {
            // we notify the teacher of a new user following a lesson..
            mqtt.publish(l.teacher.user.id + "/input", JSONB.toJson(new Message.FollowLesson(student, lesson, interests)).getBytes(), 1, false);
            // we notify other students that have a new classmate..
            User classmate = new User(student.id, student.email, student.first_name, student.last_name, student.online, null, null, null, null, null);
            for (Long student_id : l.students.keySet()) {
                if (student_id != student.id) {
                    mqtt.publish(student_id + "/input", JSONB.toJson(new Message.FollowLesson(classmate, lesson, interests)).getBytes(), 1, false);
                }
            }
        } catch (MqttException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
    }

    @Lock(LockType.WRITE)
    public void unfollow(long student, long lesson) {
        LessonManager lm = lessons.get(lesson);
        lm.unfollow(student);
        Lesson l = lm.getLesson();

        try {
            // we notify the teacher of a user unfollowing a lesson..
            mqtt.publish(l.teacher.user.id + "/input", JSONB.toJson(new Message.UnfollowLesson(student, lesson)).getBytes(), 1, false);
            // we notify other students that have lost a classmate..
            for (Long student_id : l.students.keySet()) {
                if (student_id != student) {
                    mqtt.publish(student_id + "/input", JSONB.toJson(new Message.UnfollowLesson(student, lesson)).getBytes(), 1, false);
                }
            }
        } catch (MqttException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
    }

    @Lock(LockType.WRITE)
    public void solveLesson(long lesson_id) {
        LessonManager lm = lessons.get(lesson_id);
        lm.solve();
        Lesson l = lm.getLesson();
        try {
            mqtt.publish(l.teacher.user.id + "/input/lesson-" + lesson_id + "/time", Long.toString(0).getBytes(), 1, true);
            mqtt.publish(l.teacher.user.id + "/input/lesson-" + lesson_id + "/state", Lesson.LessonState.Stopped.toString().getBytes(), 1, true);
        } catch (MqttException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
        LOG.log(Level.INFO, "Lesson {0} solved", lesson_id);
    }

    @Lock(LockType.WRITE)
    public void play(long lesson_id) {
        Lesson l = lessons.get(lesson_id).getLesson();
        l.state = Lesson.LessonState.Running;
        try {
            mqtt.publish(l.teacher.user.id + "/input/lesson-" + lesson_id + "/state", Lesson.LessonState.Running.toString().getBytes(), 1, true);
        } catch (MqttException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
    }

    @Lock(LockType.WRITE)
    public void pause(long lesson_id) {
        Lesson l = lessons.get(lesson_id).getLesson();
        l.state = Lesson.LessonState.Paused;
        try {
            mqtt.publish(l.teacher.user.id + "/input/lesson-" + lesson_id + "/state", Lesson.LessonState.Paused.toString().getBytes(), 1, true);
        } catch (MqttException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
    }

    @Lock(LockType.WRITE)
    public void stop(long lesson_id) {
        LessonManager lm = lessons.get(lesson_id);
        Lesson l = lm.getLesson();
        l.state = Lesson.LessonState.Stopped;
        lm.goTo(0);
        try {
            mqtt.publish(l.teacher.user.id + "/input/lesson-" + lesson_id + "/state", Lesson.LessonState.Stopped.toString().getBytes(), 1, true);
        } catch (MqttException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
    }

    @Lock(LockType.WRITE)
    public void go_to(long lesson_id, long time) {
        lessons.get(lesson_id).goTo(time);
    }

    @Lock(LockType.WRITE)
    public void setTime(long lesson_id, int token_id, long time) {
        lessons.get(lesson_id).setTime(token_id, time);
    }

    @Lock(LockType.WRITE)
    public void removeLesson(long lesson_id) {
        Lesson l = lessons.get(lesson_id).getLesson();
        // we notify all the students that a new lesson has been created..
        for (Long student_id : l.students.keySet()) {
            try {
                mqtt.publish(student_id + "/input", JSONB.toJson(new Message.RemoveLesson(l.id)).getBytes(), 1, false);
            } catch (MqttException ex) {
                LOG.log(Level.SEVERE, null, ex);
            }
        }
        // we delete the lesson..
        lessons.remove(lesson_id);
    }

    @Lock(LockType.WRITE)
    @Schedule(second = "*/1", minute = "*", hour = "*", persistent = false)
    public void tick() {
        if (busy.compareAndSet(false, true)) {
            lessons.values().stream().filter(lm -> lm.getLesson().state == Lesson.LessonState.Running).forEach(lm -> lm.tick());
            busy.set(false);
        }
    }

    @Lock(LockType.READ)
    public Collection<LessonManager> getLessonManagers() {
        return new ArrayList<>(lessons.values());
    }

    @Lock(LockType.READ)
    public LessonManager getLessonManager(long lesson_id) {
        return lessons.get(lesson_id);
    }

    private static boolean involves(LessonModel.Condition cond, String par) {
        switch (cond.type) {
            case And:
                return ((LessonModel.Condition.AndCondition) cond).conditions.stream().allMatch(c -> involves(c, par));
            case Or:
                return ((LessonModel.Condition.OrCondition) cond).conditions.stream().anyMatch(c -> involves(c, par));
            case Not:
                return involves(((LessonModel.Condition.NotCondition) cond).condition, par);
            case Numeric:
                String[] num_par_name = ((LessonModel.Condition.NumericCondition) cond).variable.split("\\.");
                return num_par_name[0].equals(par);
            case Nominal:
                String[] nom_par_name = ((LessonModel.Condition.NominalCondition) cond).variable.split("\\.");
                return nom_par_name[0].equals(par);
            default:
                throw new AssertionError(cond.type.name());
        }
    }

    private static boolean isSatisfied(LessonModel.Condition cond, Map<String, Map<String, String>> vals) {
        switch (cond.type) {
            case And:
                return ((LessonModel.Condition.AndCondition) cond).conditions.stream().allMatch(c -> isSatisfied(c, vals));
            case Or:
                return ((LessonModel.Condition.OrCondition) cond).conditions.stream().anyMatch(c -> isSatisfied(c, vals));
            case Not:
                return !isSatisfied(((LessonModel.Condition.NotCondition) cond).condition, vals);
            case Numeric:
                String[] num_par_name = ((LessonModel.Condition.NumericCondition) cond).variable.split("\\.");
                if (!vals.containsKey(num_par_name[0]) || !vals.get(num_par_name[0]).containsKey(num_par_name[1])) {
                    return false;
                }
                double c_numeric_val = Double.parseDouble(vals.get(num_par_name[0]).get(num_par_name[1]));
                switch (((LessonModel.Condition.NumericCondition) cond).numeric_condition_type) {
                    case GEq:
                        return c_numeric_val >= ((LessonModel.Condition.NumericCondition) cond).value;
                    case Eq:
                        return c_numeric_val == ((LessonModel.Condition.NumericCondition) cond).value;
                    case LEq:
                        return c_numeric_val <= ((LessonModel.Condition.NumericCondition) cond).value;
                    default:
                        throw new AssertionError(((LessonModel.Condition.NumericCondition) cond).numeric_condition_type.name());
                }
            case Nominal:
                String[] nom_par_name = ((LessonModel.Condition.NominalCondition) cond).variable.split("\\.");
                if (!vals.containsKey(nom_par_name[0]) || !vals.get(nom_par_name[0]).containsKey(nom_par_name[1])) {
                    return false;
                }
                return vals.get(nom_par_name[0]).get(nom_par_name[1]).equals(((LessonModel.Condition.NominalCondition) cond).value);
            default:
                throw new AssertionError(cond.type.name());
        }
    }
}
