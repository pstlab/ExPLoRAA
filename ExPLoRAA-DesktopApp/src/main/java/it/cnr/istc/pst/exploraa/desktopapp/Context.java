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

import it.cnr.istc.pst.exploraa.api.Follow;
import it.cnr.istc.pst.exploraa.api.Lesson;
import it.cnr.istc.pst.exploraa.api.LessonModel;
import it.cnr.istc.pst.exploraa.api.Message;
import it.cnr.istc.pst.exploraa.api.Parameter;
import it.cnr.istc.pst.exploraa.api.User;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Properties;
import java.util.ResourceBundle;
import java.util.Set;
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
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.scene.control.Alert;
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
    public static final Jsonb JSONB = JsonbBuilder.create(new JsonbConfig().withAdapters(Message.ADAPTER, LessonModel.ADAPTER));
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
                            Platform.runLater(() -> {
                                Alert alert = new Alert(Alert.AlertType.ERROR);
                                alert.setTitle(Context.LANGUAGE.getString("EXCEPTION"));
                                alert.setHeaderText(cause.getMessage());
                                alert.showAndWait();
                            });
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
                            case RemoveLesson:
                                // a teacher has removed a lesson for this student..
                                Message.RemoveLesson remove_lesson = (Message.RemoveLesson) m;
                                Platform.runLater(() -> following_lessons.remove(id_following_lessons.get(remove_lesson.lesson)));
                                break;
                            case FollowLesson:
                                // a new student is following a lesson of this teacher..
                                Message.FollowLesson follow_lesson = (Message.FollowLesson) m;
                                Platform.runLater(() -> id_teaching_lessons.get(follow_lesson.lesson).studentsProperty().add(new StudentContext(follow_lesson.student)));
                                break;
                            case UnfollowLesson:
                                // a student is not following a lesson of this user anymore..
                                Message.UnfollowLesson unfollow_lesson = (Message.UnfollowLesson) m;
                                Platform.runLater(() -> {
                                    id_teaching_lessons.get(unfollow_lesson.lesson).studentsProperty().remove(id_students.get(unfollow_lesson.student));
                                    Set<Long> c_students = new HashSet<>();
                                    for (TeachingLessonContext l : teaching_lessons) {
                                        for (StudentContext student : l.studentsProperty()) {
                                            c_students.add(student.getStudent().id);
                                        }
                                    }
                                    if (!c_students.contains(unfollow_lesson.student)) {
                                        students.remove(id_students.get(unfollow_lesson.student));
                                    }
                                });
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

                    if (newValue.follows != null) {
                        // we add the following lessons..
                        newValue.follows.values().forEach(follow -> following_lessons.add(new FollowingLessonContext(follow.lesson)));
                    }

                    if (newValue.models != null) {
                        // we add the available models..
                        models.addAll(newValue.models.values());
                    }

                    if (newValue.teachs != null) {
                        // we add the teaching lessons..
                        newValue.teachs.values().forEach(teach -> teaching_lessons.add(new TeachingLessonContext(teach.lesson)));
                    }

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

        following_lessons.addListener((ListChangeListener.Change<? extends FollowingLessonContext> c) -> {
            while (c.next()) {
                c.getAddedSubList().forEach(flc -> {
                    id_following_lessons.put(flc.getLesson().id, flc);
                    flc.stimuliProperty().addAll(flc.getLesson().stimuli);
                    if (!id_teachers.containsKey(flc.getLesson().teacher.user.id)) {
                        teachers.add(new TeacherContext(flc.getLesson().teacher.user));
                    }
                    try {
                        // we subscribe to the lesson's time..
                        mqtt.subscribe(flc.getLesson().teacher.user.id + "/input/lesson-" + flc.getLesson().id + "/time", (String topic, MqttMessage message) -> {
                            Platform.runLater(() -> flc.timeProperty().setValue(Long.parseLong(new String(message.getPayload()))));
                        });
                        // we subscribe to the lesson's state..
                        mqtt.subscribe(flc.getLesson().teacher.user.id + "/input/lesson-" + flc.getLesson().id + "/state", (String topic, MqttMessage message) -> {
                            Platform.runLater(() -> flc.stateProperty().setValue(Lesson.LessonState.valueOf(new String(message.getPayload()))));
                        });
                    } catch (MqttException ex) {
                        LOG.log(Level.SEVERE, null, ex);
                    }
                });
                c.getRemoved().forEach(flc -> {
                    id_following_lessons.remove(flc.getLesson().id);
                    stimuli.removeAll(flc.stimuliProperty());
                    flc.stimuliProperty().clear();
                    if (mqtt.isConnected()) {
                        try {
                            // we subscribe from the lesson's time and state..
                            mqtt.unsubscribe(flc.getLesson().teacher.user.id + "/input/lesson-" + flc.getLesson().id + "/time");
                            mqtt.unsubscribe(flc.getLesson().teacher.user.id + "/input/lesson-" + flc.getLesson().id + "/state");
                        } catch (MqttException ex) {
                            LOG.log(Level.SEVERE, null, ex);
                        }
                    }
                });
                if (!c.getRemoved().isEmpty()) {
                    Set<Long> c_teachers = new HashSet<>();
                    for (FollowingLessonContext l : following_lessons) {
                        c_teachers.add(l.getLesson().teacher.user.id);
                    }
                    Set<Long> to_remove_teachers = new HashSet<>();
                    for (Long teacher_id : id_teachers.keySet()) {
                        if (!c_teachers.contains(teacher_id)) {
                            to_remove_teachers.add(teacher_id);
                        }
                    }
                    for (Long to_remove_student : to_remove_teachers) {
                        teachers.remove(id_teachers.get(to_remove_student));
                    }
                }
            }
        });

        teachers.addListener((ListChangeListener.Change<? extends TeacherContext> c) -> {
            while (c.next()) {
                c.getAddedSubList().forEach(tch_ctx -> {
                    try {
                        mqtt.subscribe(tch_ctx.getTeacher().id + "/output/on-line", (String topic, MqttMessage message) -> Platform.runLater(() -> tch_ctx.onlineProperty().set(Boolean.parseBoolean(new String(message.getPayload())))));
                    } catch (MqttException ex) {
                        LOG.log(Level.SEVERE, null, ex);
                    }
                    id_teachers.put(tch_ctx.getTeacher().id, tch_ctx);
                });
                c.getRemoved().forEach(tch_ctx -> {
                    try {
                        if (mqtt.isConnected()) { // we might be removing teachers as a consequence of a connection loss..
                            mqtt.unsubscribe(tch_ctx.getTeacher().id + "/output/on-line");
                        }
                    } catch (MqttException ex) {
                        LOG.log(Level.SEVERE, null, ex);
                    }
                    id_teachers.remove(tch_ctx.getTeacher().id);
                });
            }
        });

        teaching_lessons.addListener((ListChangeListener.Change<? extends TeachingLessonContext> c) -> {
            while (c.next()) {
                c.getAddedSubList().forEach(tlc -> {
                    id_teaching_lessons.put(tlc.getLesson().id, tlc);
                    if (tlc.getLesson().students != null) {
                        for (Follow follow : tlc.getLesson().students.values()) {
                            tlc.studentsProperty().add(id_students.containsKey(follow.user.id) ? id_students.get(follow.user.id) : new StudentContext(follow.user));
                        }
                    }
                    try {
                        // we subscribe to the lesson's time..
                        mqtt.subscribe(user.get().id + "/input/lesson-" + tlc.getLesson().id + "/time", (String topic, MqttMessage message) -> {
                            Platform.runLater(() -> tlc.timeProperty().setValue(Long.parseLong(new String(message.getPayload()))));
                        });
                        // we subscribe to the lesson's state..
                        mqtt.subscribe(user.get().id + "/input/lesson-" + tlc.getLesson().id + "/state", (String topic, MqttMessage message) -> {
                            Platform.runLater(() -> tlc.stateProperty().setValue(Lesson.LessonState.valueOf(new String(message.getPayload()))));
                        });
                    } catch (MqttException ex) {
                        LOG.log(Level.SEVERE, null, ex);
                    }
                });
                c.getRemoved().forEach(tlc -> {
                    id_teaching_lessons.remove(tlc.getLesson().id);
                    if (user.isNotNull().get() && mqtt.isConnected()) {
                        try {
                            // we unsubscribe from the lesson's time and state..
                            mqtt.unsubscribe(user.get().id + "/input/lesson-" + tlc.getLesson().id + "/time");
                            mqtt.unsubscribe(user.get().id + "/input/lesson-" + tlc.getLesson().id + "/state");
                        } catch (MqttException ex) {
                            LOG.log(Level.SEVERE, null, ex);
                        }
                    }
                });
                if (!c.getRemoved().isEmpty()) {
                    Set<Long> c_students = new HashSet<>();
                    for (TeachingLessonContext l : teaching_lessons) {
                        for (StudentContext student : l.studentsProperty()) {
                            c_students.add(student.getStudent().id);
                        }
                    }
                    Set<Long> to_remove_students = new HashSet<>();
                    for (Long student_id : id_students.keySet()) {
                        if (!c_students.contains(student_id)) {
                            to_remove_students.add(student_id);
                        }
                    }
                    for (Long to_remove_student : to_remove_students) {
                        students.remove(id_students.get(to_remove_student));
                    }
                }
            }
        });

        students.addListener((ListChangeListener.Change<? extends StudentContext> c) -> {
            while (c.next()) {
                for (StudentContext std_ctx : c.getAddedSubList()) {
                    long student_id = std_ctx.getStudent().id;
                    try {
                        // we subscribe to be notified whether the student gets online/offline..
                        mqtt.subscribe(student_id + "/output/on-line", (String topic, MqttMessage message) -> Platform.runLater(() -> std_ctx.onlineProperty().set(Boolean.parseBoolean(new String(message.getPayload())))));
                        // we subscribe/unsubscribe to the student's added/removed parameters..
                        std_ctx.parameterTypesProperty().addListener((ListChangeListener.Change<? extends Parameter> c1) -> {
                            while (c1.next()) {
                                // we subscribe to the new user's parameters..
                                c1.getAddedSubList().forEach(par -> {
                                    try {
                                        mqtt.subscribe(std_ctx.getStudent().id + "/output/" + par.name, (String topic, MqttMessage message) -> {
                                            Map<String, String> c_par_vals = JSONB.fromJson(new String(message.getPayload()), new HashMap<String, String>() {
                                            }.getClass().getGenericSuperclass());
                                            Platform.runLater(() -> std_ctx.setParameterValue(par.name, c_par_vals));
                                        });
                                    } catch (MqttException ex) {
                                        LOG.log(Level.SEVERE, null, ex);
                                    }
                                });
                                // we unsubscribe from the removed parameters..
                                if (mqtt.isConnected()) {
                                    c1.getRemoved().forEach(par -> {
                                        try {
                                            mqtt.unsubscribe(student_id + "/output/" + par.name);
                                        } catch (MqttException ex) {
                                            LOG.log(Level.SEVERE, null, ex);
                                        }
                                    });
                                }
                            }
                        });
                        if (std_ctx.isOnline()) {
                            // we add the current student's parameters..
                            // notice that in case the student is offline, the parameters will be added by the subscription to the student's output..
                            std_ctx.parameterTypesProperty().addAll(std_ctx.getStudent().par_types.values());
                        }
                        // we subscribe to the student's output..
                        mqtt.subscribe(std_ctx.getStudent().id + "/output", (String topic, MqttMessage message) -> {
                            Message m = JSONB.fromJson(new String(message.getPayload()), Message.class);
                            switch (m.message_type) {
                                case NewParameter:
                                    Message.NewParameter new_parameter = JSONB.fromJson(new String(message.getPayload()), Message.NewParameter.class);
                                    Platform.runLater(() -> std_ctx.parameterTypesProperty().add(new_parameter.parameter));
                                    break;
                                case RemoveParameter:
                                    Message.RemoveParameter remove_parameter = JSONB.fromJson(new String(message.getPayload()), Message.RemoveParameter.class);
                                    Platform.runLater(() -> std_ctx.parameterTypesProperty().remove(std_ctx.getParameter(remove_parameter.parameter)));
                                    break;
                                case Answer:
                                    break;
                                default:
                                    throw new AssertionError(m.message_type.name());
                            }
                        });
                        std_ctx.getStudent().par_values.entrySet().forEach(par_val -> std_ctx.setParameterValue(par_val.getKey(), par_val.getValue()));
                    } catch (MqttException ex) {
                        LOG.log(Level.SEVERE, null, ex);
                    }
                    id_students.put(std_ctx.getStudent().id, std_ctx);
                }
                c.getRemoved().forEach(std_ctx -> {
                    try {
                        if (mqtt.isConnected()) {
                            mqtt.unsubscribe(std_ctx.getStudent().id + "/output/on-line");
                        }
                        std_ctx.parameterTypesProperty().clear();
                    } catch (MqttException ex) {
                        LOG.log(Level.SEVERE, null, ex);
                    }
                    id_students.remove(std_ctx.getStudent().id);
                });
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

    public StudentContext getStudent(final long id) {
        return id_students.get(id);
    }

    public void login(String email, String password) {
        User u = resource.login(email, password);
        u.par_types = load_pars();
        u.par_values = load_par_vals();
        user.set(u);
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
        l.model = model;
        teaching_lessons.add(new TeachingLessonContext(l));
        resource.solve(l.id);
    }

    public void removeLesson(TeachingLessonContext l_ctx) {
        resource.delete_lesson(l_ctx.getLesson().id);
        teaching_lessons.remove(l_ctx);
    }

    public Collection<Lesson> getLessons() {
        return resource.get_lessons();
    }

    public void followLesson(Lesson lesson, Set<String> interests) {
        resource.follow(user.get().id, lesson.id, JSONB.toJson(interests));
        following_lessons.add(new FollowingLessonContext(lesson));
    }

    public void unfollowLesson(Lesson lesson) {
        resource.unfollow(user.get().id, lesson.id);
        following_lessons.remove(id_following_lessons.get(lesson.id));
    }

    public void answerQuestion(Message.Stimulus.QuestionStimulus question, int answer) {
        resource.answer_question(question.lesson_id, question.id, answer);
    }

    public void setTime(Lesson lesson, TeachingLessonContext.TokenRow row, long time) {
        resource.set_time(lesson.id, row.getId(), time);
    }

    public void play(Lesson lesson) {
        resource.play(lesson.id);
    }

    public void pause(Lesson lesson) {
        resource.pause(lesson.id);
    }

    public void stop(Lesson lesson) {
        resource.stop(lesson.id);
    }

    public void goTo(Lesson lesson, long time) {
        resource.go_to(lesson.id, time);
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
