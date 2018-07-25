/*
 * Copyright (C) 2018 Your Organisation
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
package it.cnr.istc.pst.exploraa.desktopapp;

import it.cnr.istc.pst.exploraa.api.Lesson;
import it.cnr.istc.pst.exploraa.api.LessonModel;
import it.cnr.istc.pst.exploraa.api.Message;
import it.cnr.istc.pst.exploraa.api.Parameter;
import it.cnr.istc.pst.exploraa.api.User;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.ResourceBundle;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;
import javafx.application.Platform;
import javafx.beans.Observable;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.stage.Stage;
import javax.json.bind.Jsonb;
import javax.json.bind.JsonbBuilder;
import javax.json.bind.JsonbConfig;
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
public class Context {

    private static final Logger LOG = Logger.getLogger(Context.class.getName());
    public static final Jsonb JSONB = JsonbBuilder.create(new JsonbConfig().withAdapters(Message.ADAPTER));
    public static final ResourceBundle LANGUAGE = ResourceBundle.getBundle("language");
    private static ScheduledExecutorService EXECUTOR;
    private static Context ctx;

    public static Context getContext() {
        if (ctx == null) {
            ctx = new Context();
        }
        return ctx;
    }
    private final Properties properties = new Properties();
    private final ExPLoRAAResource resource;
    private MqttClient mqtt;
    private Stage stage;
    /**
     * The current user.
     */
    private final ObjectProperty<User> user = new SimpleObjectProperty<>();
    /**
     * The current user's parameter types.
     */
    private final ObservableList<Parameter> par_types = FXCollections.observableArrayList();
    private final Map<String, Parameter> id_par_types = new HashMap<>();
    /**
     * The current user's parameter values.
     */
    private final Map<String, Map<String, ParameterValue>> par_vals = new HashMap<>();
    /**
     * The current user's parameter values as a list, to be displayed on tables.
     * Notice that each parameter can aggregate more than a single value.
     */
    private final ObservableList<ParameterValue> par_values = FXCollections.observableArrayList();
    /**
     * The received stimuli.
     */
    private final ObservableList<Message.Stimulus> stimuli = FXCollections.observableArrayList();
    /**
     * The lessons followed as a student.
     */
    private final ObservableList<FollowingLessonContext> following_lessons = FXCollections.observableArrayList(l_ctx -> new Observable[]{l_ctx.stateProperty()});
    private final Map<Long, FollowingLessonContext> id_following_lessons = new HashMap<>();
    /**
     * The followed teachers.
     */
    private final ObservableList<TeacherContext> teachers = FXCollections.observableArrayList(tch_ctx -> new Observable[]{tch_ctx.onlineProperty()});
    private final Map<Long, TeacherContext> id_teachers = new HashMap<>();
    /**
     * The lesson models associated to the teacher.
     */
    private final ObservableList<LessonModel> models = FXCollections.observableArrayList();
    /**
     * The lessons followed as a teacher.
     */
    private final ObservableList<TeachingLessonContext> teaching_lessons = FXCollections.observableArrayList(l_ctx -> new Observable[]{l_ctx.stateProperty()});
    private final Map<Long, TeachingLessonContext> id_teaching_lessons = new HashMap<>();
    /**
     * The following students.
     */
    private final ObservableList<StudentContext> students = FXCollections.observableArrayList(std_ctx -> new Observable[]{std_ctx.onlineProperty()});
    private final Map<Long, StudentContext> id_students = new HashMap<>();

    private Context() {
        try {
            properties.load(Thread.currentThread().getContextClassLoader().getResourceAsStream("config.properties"));
        } catch (IOException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
        this.resource = new ExPLoRAAResource(properties);
        user.addListener((ObservableValue<? extends User> observable, User oldValue, User newValue) -> {
            if (oldValue != null) {
                // we clear the current data..
                try {
                    par_values.clear();
                    par_vals.clear();
                    if (mqtt.isConnected()) { // a user might become null as a consequence of a connection loss..
                        for (Parameter par : par_types) {
                            // we broadcast the lost of a parameter..
                            mqtt.publish(oldValue.id + "/output", JSONB.toJson(new Message.RemoveParameter(par.name)).getBytes(), 1, false);
                        }
                    }
                    par_types.clear();
                    stimuli.clear();
                    if (mqtt.isConnected()) {
                        for (FollowingLessonContext l_ctx : following_lessons) {
                            // we unsubscribe from the lesson's time and state..
                            mqtt.unsubscribe(oldValue.id + "/input/lesson-" + l_ctx.getLesson().id + "/time");
                            mqtt.unsubscribe(oldValue.id + "/input/lesson-" + l_ctx.getLesson().id + "/state");
                        }
                    }
                    following_lessons.clear();
                    teachers.clear();
                    models.clear();
                    for (TeachingLessonContext l_ctx : teaching_lessons) {
                        l_ctx.tokensProperty().clear();
                        // we unsubscribe from the lesson's time and state..
                        if (mqtt.isConnected()) {
                            mqtt.unsubscribe(oldValue.id + "/input/lesson-" + l_ctx.getLesson().id + "/time");
                            mqtt.unsubscribe(oldValue.id + "/input/lesson-" + l_ctx.getLesson().id + "/state");
                        }
                    }
                    teaching_lessons.clear();
                    students.clear();
                    if (mqtt.isConnected()) {
                        mqtt.disconnect();
                    }
                    mqtt.close();
                } catch (MqttException ex) {
                    LOG.log(Level.SEVERE, null, ex);
                }

                EXECUTOR.shutdown();
            }
            if (newValue != null) {
                // we set up a new user..
                try {
                    mqtt = new MqttClient("tcp://" + properties.getProperty("host", "localhost") + ":" + properties.getProperty("mqtt-port", "1883"), String.valueOf(newValue.id), new MemoryPersistence());
                    mqtt.setCallback(new MqttCallback() {
                        @Override
                        public void connectionLost(Throwable cause) {
                            LOG.log(Level.SEVERE, null, cause);
                            Platform.runLater(() -> user.set(null));
                        }

                        @Override
                        public void messageArrived(String topic, MqttMessage message) throws Exception {
                            LOG.log(Level.WARNING, "message arrived: {0} - {1}", new Object[]{topic, message});
                        }

                        @Override
                        public void deliveryComplete(IMqttDeliveryToken token) {
                        }
                    });

                    MqttConnectOptions options = new MqttConnectOptions();
                    options.setCleanSession(true);
                    options.setAutomaticReconnect(true);
                    mqtt.connect(options);

                    mqtt.subscribe(newValue.id + "/input", (String topic, MqttMessage message) -> {
                        LOG.log(Level.INFO, "message arrived: {0} - {1}", new Object[]{topic, message});
                        Message m = JSONB.fromJson(new String(message.getPayload()), Message.class);
                        switch (m.message_type) {
                            case NewLesson:
                                // a teacher has created a new lesson for this student..
                                Message.NewLesson new_lesson = (Message.NewLesson) m;
                                Platform.runLater(() -> following_lessons.add(new FollowingLessonContext(new_lesson.lesson)));
                                break;
                            case RemoveLesson:
                                // a teacher has removed a new lesson for this student..
                                Message.RemoveLesson remove_lesson = (Message.RemoveLesson) m;
                                Platform.runLater(() -> following_lessons.remove(id_following_lessons.get(remove_lesson.lesson)));
                                break;
                            case Token:
                                // a new token has been created for a teaching lesson..
                                Message.Token token = (Message.Token) m;
                                Platform.runLater(() -> id_teaching_lessons.get(token.lesson_id).tokensProperty().add(new TeachingLessonContext.TokenRow(token.id, id_teaching_lessons.get(token.lesson_id).timeProperty(), token.min, token.max, token.time, token.refEvent)));
                                break;
                            case TokenUpdate:
                                // a token of a teaching lesson has been updated..
                                Message.TokenUpdate token_update = (Message.TokenUpdate) m;
                                Platform.runLater(() -> {
                                    id_teaching_lessons.get(token_update.lesson_id).getToken(token_update.id).timeProperty().setValue(token_update.time);
                                    id_teaching_lessons.get(token_update.lesson_id).getToken(token_update.id).minProperty().setValue(token_update.min);
                                    id_teaching_lessons.get(token_update.lesson_id).getToken(token_update.id).maxProperty().setValue(token_update.max);
                                });
                                break;
                            case RemoveToken:
                                // a token of a teaching lesson has been removed..
                                Message.RemoveToken remove_token = (Message.RemoveToken) m;
                                Platform.runLater(() -> id_teaching_lessons.get(remove_token.lesson_id).tokensProperty().remove(id_teaching_lessons.get(remove_token.lesson_id).getToken((int) remove_token.id)));
                                break;
                            case Stimulus:
                                // a new event has been created for a following lesson..
                                Message.Stimulus stimulus = (Message.Stimulus) m;
                                Platform.runLater(() -> id_following_lessons.get(stimulus.lesson_id).stimuliProperty().add(stimulus));
                                break;
                            case RemoveStimulus:
                                // an event has been removed for a following lesson..
                                Message.RemoveStimulus remove_stimulus = (Message.RemoveStimulus) m;
                                Platform.runLater(() -> id_following_lessons.get(remove_stimulus.lesson_id).stimuliProperty().removeIf(st -> st.id == remove_stimulus.id));
                                break;
                            case Answer:
                                break;
                            default:
                                throw new AssertionError(m.message_type.name());
                        }
                    });

                    for (Parameter par : newValue.par_types.values()) {
                        par_types.add(par);
                        // we broadcast the existence of a new parameter..
                        mqtt.publish(newValue.id + "/output", JSONB.toJson(new Message.NewParameter(par)).getBytes(), 1, false);
                    }
                    for (Map.Entry<String, Map<String, String>> par_val : newValue.par_values.entrySet()) {
                        Map<String, ParameterValue> c_vals = new HashMap<>();
                        par_val.getValue().entrySet().forEach(val -> {
                            ParameterValue val_prop = new ParameterValue(par_val.getKey() + "." + val.getKey(), val.getValue());
                            c_vals.put(val.getKey(), val_prop);
                            par_values.add(val_prop);
                        });
                        par_vals.put(par_val.getKey(), c_vals);
                        // we broadcast the the new value of the parameter..
                        mqtt.publish(newValue.id + "/output/" + par_val.getKey(), JSONB.toJson(par_val.getValue()).getBytes(), 1, true);
                    }
                } catch (MqttException ex) {
                    LOG.log(Level.SEVERE, null, ex);
                }

                // we simulate the passing of time..
                EXECUTOR = Executors.newSingleThreadScheduledExecutor();
                EXECUTOR.scheduleAtFixedRate(() -> students.forEach(std_ctx -> std_ctx.parametersProperty().forEach(par_val -> Platform.runLater(() -> par_val.updatesProperty().add(new ParUpdate(System.currentTimeMillis(), par_val.value.get()))))), 0, 1, TimeUnit.SECONDS);
            }
        });
    }

    public Properties getProperties() {
        return properties;
    }

    public Stage getStage() {
        return stage;
    }

    public void setStage(Stage stage) {
        this.stage = stage;
    }

    public User getUser() {
        return user.get();
    }

    public void setUser(User user) {
        this.user.set(user);
    }

    public ObjectProperty<User> userProperty() {
        return user;
    }

    public ObservableList<Parameter> parameterTypesProperty() {
        return par_types;
    }

    public Parameter getParameter(String par_name) {
        return id_par_types.get(par_name);
    }

    public ObservableList<Message.Stimulus> stimuliProperty() {
        return stimuli;
    }

    public ObservableList<FollowingLessonContext> followingLessonsProperty() {
        return following_lessons;
    }

    public ObservableList<TeacherContext> teachersProperty() {
        return teachers;
    }

    public ObservableList<LessonModel> modelsProperty() {
        return models;
    }

    public ObservableList<TeachingLessonContext> teachingLessonsProperty() {
        return teaching_lessons;
    }

    public ObservableList<StudentContext> studentsProperty() {
        return students;
    }

    public void login(String email, String password) {
        User u = resource.login(email, password);
        u.par_types = load_pars();
        u.par_values = load_par_vals();
        user.set(u);

        // we add the following lessons..
        u.follows.values().forEach(follow -> following_lessons.add(new FollowingLessonContext(follow.lesson)));

        // we add the available models..
        models.addAll(u.models.values());

        // we add the teaching lessons..
        u.teachs.values().forEach(teach -> teaching_lessons.add(new TeachingLessonContext(teach.lesson, teach.lesson.model)));
    }

    public void logout() {
        user.set(null);
    }

    public void new_user(String email, String password, String first_name, String last_name) {
        User u = resource.new_user(email, password, first_name, last_name);
        u.par_types = load_pars();
        u.par_values = load_par_vals();
        user.set(u);
    }

    public void addLesson(String name, LessonModel model) {
        Lesson l = model.id != null ? resource.new_lesson(user.get().id, name, model.id) : resource.new_lesson(user.get().id, name, JSONB.toJson(model));
        teaching_lessons.add(new TeachingLessonContext(l, model));
        resource.solve(l.id);
    }

    public void removeLesson(TeachingLessonContext l_ctx) {
        resource.delete_lesson(l_ctx.getLesson().id);
        teaching_lessons.remove(l_ctx);
    }

    private static Map<String, Parameter> load_pars() {
        Collection<Parameter> pars = JSONB.fromJson(Context.class.getResourceAsStream("/parameters/types.json"), new ArrayList<Parameter>() {
        }.getClass().getGenericSuperclass());
        return pars.stream().collect(Collectors.toMap(p -> p.name, p -> p));
    }

    private static Map<String, Map<String, String>> load_par_vals() {
        Map<String, Map<String, String>> par_vals = JSONB.fromJson(Context.class.getResourceAsStream("/parameters/values.json"), new HashMap<String, Map<String, String>>() {
        }.getClass().getGenericSuperclass());
        return par_vals;
    }

    public static class ParameterValue {

        private final StringProperty name;
        private final StringProperty value;
        private final ObservableList<ParUpdate> updates = FXCollections.observableArrayList();

        ParameterValue(String name, String value) {
            this.name = new SimpleStringProperty(name);
            this.value = new SimpleStringProperty(value);
            this.updates.add(new ParUpdate(System.currentTimeMillis(), value));
            this.value.addListener((ObservableValue<? extends String> observable, String oldValue, String newValue) -> updates.add(new ParUpdate(System.currentTimeMillis(), newValue)));
        }

        public StringProperty nameProperty() {
            return name;
        }

        public StringProperty valueProperty() {
            return value;
        }

        public ObservableList<ParUpdate> updatesProperty() {
            return updates;
        }
    }

    public static class ParUpdate {

        public final long time;
        public final String new_value;

        public ParUpdate(long time, String new_value) {
            this.time = time;
            this.new_value = new_value;
        }
    }
}
