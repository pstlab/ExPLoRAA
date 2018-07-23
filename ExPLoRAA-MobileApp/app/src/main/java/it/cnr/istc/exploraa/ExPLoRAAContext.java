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
package it.cnr.istc.exploraa;

import android.Manifest;
import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.Context;
import android.content.pm.PackageManager;
import android.location.Location;
import android.location.LocationListener;
import android.location.LocationManager;
import android.os.AsyncTask;
import android.os.Bundle;
import android.support.annotation.NonNull;
import android.support.v4.content.ContextCompat;
import android.util.Log;
import android.util.LongSparseArray;
import android.widget.Toast;

import com.google.gson.Gson;

import org.eclipse.paho.client.mqttv3.IMqttDeliveryToken;
import org.eclipse.paho.client.mqttv3.IMqttMessageListener;
import org.eclipse.paho.client.mqttv3.MqttCallback;
import org.eclipse.paho.client.mqttv3.MqttClient;
import org.eclipse.paho.client.mqttv3.MqttConnectOptions;
import org.eclipse.paho.client.mqttv3.MqttException;
import org.eclipse.paho.client.mqttv3.MqttMessage;
import org.eclipse.paho.client.mqttv3.persist.MemoryPersistence;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutionException;

import it.cnr.istc.exploraa.api.ExPLoRAA;
import it.cnr.istc.exploraa.api.Follow;
import it.cnr.istc.exploraa.api.LessonModel;
import it.cnr.istc.exploraa.api.Message;
import it.cnr.istc.exploraa.api.Parameter;
import it.cnr.istc.exploraa.api.Teach;
import it.cnr.istc.exploraa.api.User;
import retrofit2.Response;
import retrofit2.Retrofit;
import retrofit2.converter.gson.GsonConverterFactory;

/**
 * @author Riccardo De Benedictis
 */
public class ExPLoRAAContext implements LocationListener {

    private static final String TAG = "LECTurEContext";
    private static final Gson GSON = new Gson();
    private static ExPLoRAAContext instance;
    private final ExPLoRAA resource;
    /**
     * The current user's parameter types.
     */
    private final List<Parameter> par_types = new ArrayList<>();
    private final Map<String, Parameter> id_par_types = new HashMap<>();
    /**
     * The current user's parameter values.
     */
    private final Map<String, Map<String, String>> par_vals = new HashMap<>();
    /**
     * The received stimuli.
     */
    private final List<Message.Stimulus> stimuli = new ArrayList<>();
    /**
     * The lessons followed as a student.
     */
    private final List<FollowingLessonContext> following_lessons = new ArrayList<>();
    private final LongSparseArray<FollowingLessonContext> id_following_lessons = new LongSparseArray<>();
    /**
     * The followed teachers.
     */
    private final List<TeacherContext> teachers = new ArrayList<>();
    private final LongSparseArray<TeacherContext> id_teachers = new LongSparseArray<>();
    /**
     * The lesson models associated to the teacher.
     */
    private final List<LessonModel> models = new ArrayList<>();
    /**
     * The lessons followed as a teacher.
     */
    private final List<TeachingLessonContext> teaching_lessons = new ArrayList<>();
    private final LongSparseArray<TeachingLessonContext> id_teaching_lessons = new LongSparseArray<>();
    /**
     * The following students.
     */
    private final List<StudentContext> students = new ArrayList<>();
    private final LongSparseArray<StudentContext> id_students = new LongSparseArray<>();
    private MqttClient mqtt;
    /**
     * The current user.
     */
    private User user;
    private final Collection<ContextListener> listeners = new ArrayList<>();

    private ExPLoRAAContext() {
        Retrofit retrofit = new Retrofit.Builder().baseUrl("http://" + BuildConfig.HOST + ":" + BuildConfig.SERVICE_PORT + "/ExPLoRAA/resources/").addConverterFactory(GsonConverterFactory.create(GSON)).build();
        resource = retrofit.create(ExPLoRAA.class);
    }

    public static ExPLoRAAContext getInstance() {
        if (instance == null) instance = new ExPLoRAAContext();
        return instance;
    }

    public User getUser() {
        return user;
    }

    public void setUser(@NonNull final Context ctx, User user) {
        if (this.user != user) {
            if (this.user != null) {
                // we clear the current data..
                try {
                    par_vals.clear();
                    // a user might become null as a consequence of a connection loss..
                    // we broadcast the lost of a parameter..
                    if (mqtt.isConnected()) for (Parameter par : par_types)
                        mqtt.publish(this.user.id + "/output", GSON.toJson(new Message.RemoveParameter(id_par_types.get(par.name))).getBytes(), 1, false);
                    ((Activity) ctx).runOnUiThread(new Runnable() {
                        public void run() {
                            if (ContextCompat.checkSelfPermission(ctx, Manifest.permission.ACCESS_FINE_LOCATION) == PackageManager.PERMISSION_GRANTED)
                                ((LocationManager) ctx.getSystemService(Context.LOCATION_SERVICE)).removeUpdates(ExPLoRAAContext.this);
                        }
                    });
                    par_types.clear();
                    id_par_types.clear();
                    stimuli.clear();
                    if (mqtt.isConnected()) for (FollowingLessonContext l_ctx : following_lessons) {
                        // we unsubscribe from the lesson's time and state..
                        mqtt.unsubscribe(this.user.id + "/input/lesson-" + l_ctx.getLesson().id + "/time");
                        mqtt.unsubscribe(this.user.id + "/input/lesson-" + l_ctx.getLesson().id + "/state");
                    }
                    following_lessons.clear();
                    id_following_lessons.clear();
                    for (ContextListener listener : listeners) listener.followingLessonsCleared();
                    teachers.clear();
                    id_teachers.clear();
                    for (ContextListener listener : listeners) listener.teachersCleared();
                    models.clear();
                    // we unsubscribe from the lesson's time and state..
                    for (TeachingLessonContext l_ctx : teaching_lessons)
                        if (mqtt.isConnected()) {
                            mqtt.unsubscribe(this.user.id + "/input/lesson-" + l_ctx.getLesson().id + "/time");
                            mqtt.unsubscribe(this.user.id + "/input/lesson-" + l_ctx.getLesson().id + "/state");
                        }
                    teaching_lessons.clear();
                    id_teaching_lessons.clear();
                    for (ContextListener listener : listeners) listener.teachingLessonsCleared();
                    students.clear();
                    id_students.clear();
                    for (ContextListener listener : listeners) listener.studentsCleared();
                    if (mqtt.isConnected()) mqtt.disconnect();
                    mqtt.close();
                } catch (MqttException ex) {
                    Log.e(TAG, null, ex);
                }
            }

            if (user != null) {
                try {
                    mqtt = new MqttClient("tcp://" + BuildConfig.HOST + ":" + BuildConfig.MQTT_PORT, String.valueOf(user.id), new MemoryPersistence());
                    mqtt.setCallback(new MqttCallback() {
                        @Override
                        public void connectionLost(Throwable cause) {
                            Log.e(TAG, "Connection lost..", cause);
                        }

                        @Override
                        public void messageArrived(String topic, MqttMessage message) {
                            Log.w(TAG, "Message arrived: " + topic + " - " + message);
                        }

                        @Override
                        public void deliveryComplete(IMqttDeliveryToken token) {
                        }
                    });

                    MqttConnectOptions options = new MqttConnectOptions();
                    options.setCleanSession(true);
                    options.setAutomaticReconnect(true);
                    mqtt.connect(options);
                    Log.i(TAG, "Connected to the MQTT broker..");
                    mqtt.subscribe(user.id + "/input", new IMqttMessageListener() {
                        @Override
                        public void messageArrived(String topic, MqttMessage message) {
                            Log.w(TAG, "Message arrived: " + topic + " - " + message);
                            Message m = GSON.fromJson(new String(message.getPayload()), Message.class);
                            switch (m.message_type) {
                                case NewLesson:
                                    // a teacher has created a new lesson for this student..
                                    Message.NewLesson new_lesson = GSON.fromJson(new String(message.getPayload()), Message.NewLesson.class);
                                    break;
                                case RemoveLesson:
                                    // a teacher has removed a new lesson for this student..
                                    Message.RemoveLesson lost_lesson = GSON.fromJson(new String(message.getPayload()), Message.RemoveLesson.class);
                                    break;
                                case Token:
                                    // a new token has been created for a teaching lesson..
                                    Message.Token token = GSON.fromJson(new String(message.getPayload()), Message.Token.class);
                                    break;
                                case TokenUpdate:
                                    // a token of a teaching lesson has been updated..
                                    Message.TokenUpdate token_update = GSON.fromJson(new String(message.getPayload()), Message.TokenUpdate.class);
                                    break;
                                case RemoveToken:
                                    // a token of a teaching lesson has been removed..
                                    Message.RemoveToken remove_token = GSON.fromJson(new String(message.getPayload()), Message.RemoveToken.class);
                                    break;
                                case Stimulus:
                                    // a new stimulus has been created for a following lesson..
                                    Message.Stimulus event = GSON.fromJson(new String(message.getPayload()), Message.Stimulus.class);
                                    break;
                                case RemoveStimulus:
                                    // a stimulus has been removed for a following lesson..
                                    Message.RemoveStimulus hide_event = GSON.fromJson(new String(message.getPayload()), Message.RemoveStimulus.class);
                                    break;
                                case Answer:
                                    break;
                                default:
                                    throw new AssertionError(m.message_type.name());
                            }
                        }
                    });

                    for (Parameter par : user.par_types.values()) {
                        par_types.add(par);
                        // we broadcast the existence of a new parameter..
                        mqtt.publish(user.id + "/output", GSON.toJson(new Message.NewParameter(par)).getBytes(), 1, false);
                    }
                    ((Activity) ctx).runOnUiThread(new Runnable() {
                        public void run() {
                            if (ContextCompat.checkSelfPermission(ctx, Manifest.permission.ACCESS_FINE_LOCATION) == PackageManager.PERMISSION_GRANTED)
                                ((LocationManager) ctx.getSystemService(Context.LOCATION_SERVICE)).requestLocationUpdates(LocationManager.GPS_PROVIDER, 0, 0, ExPLoRAAContext.this);
                        }
                    });
                    for (Map.Entry<String, Map<String, String>> par_val : user.par_values.entrySet()) {
                        par_vals.put(par_val.getKey(), par_val.getValue());
                        // we broadcast the the new value of the parameter..
                        mqtt.publish(user.id + "/output/" + par_val.getKey(), GSON.toJson(par_val.getValue()).getBytes(), 1, true);
                    }
                } catch (MqttException e) {
                    Log.w(TAG, "MQTT Connection failed..", e);
                }
            }
            this.user = user;
        }
    }

    public List<Message.Stimulus> getStimuli() {
        return Collections.unmodifiableList(stimuli);
    }

    public List<FollowingLessonContext> getFollowingLessons() {
        return Collections.unmodifiableList(following_lessons);
    }

    private void addFollowingLesson(FollowingLessonContext l) {
        following_lessons.add(l);
        id_following_lessons.put(l.getLesson().id, l);
        for (ContextListener listener : listeners) listener.followingLessonAdded(l);
    }

    private void removeFollowingLesson(FollowingLessonContext l) {
        following_lessons.remove(l);
        id_following_lessons.remove(l.getLesson().id);
        for (ContextListener listener : listeners) listener.followingLessonRemoved(l);
    }

    public List<TeacherContext> getTeachers() {
        return Collections.unmodifiableList(teachers);
    }

    private void addTeacher(TeacherContext t) {
        teachers.add(t);
        id_teachers.put(t.getTeacher().id, t);
        for (ContextListener listener : listeners) listener.teacherAdded(t);
    }

    private void removeTeacher(TeacherContext t) {
        teachers.remove(t);
        id_teachers.remove(t.getTeacher().id);
        for (ContextListener listener : listeners) listener.teacherRemoved(t);
    }

    public List<LessonModel> getModels() {
        return Collections.unmodifiableList(models);
    }

    public List<TeachingLessonContext> getTeachingLessons() {
        return Collections.unmodifiableList(teaching_lessons);
    }

    private void addTeachingLesson(TeachingLessonContext l) {
        teaching_lessons.add(l);
        id_teaching_lessons.put(l.getLesson().id, l);
        for (ContextListener listener : listeners) listener.teachingLessonAdded(l);
    }

    private void removeTeachingLesson(TeachingLessonContext l) {
        teaching_lessons.remove(l);
        id_teaching_lessons.remove(l.getLesson().id);
        for (ContextListener listener : listeners) listener.teachingLessonRemoved(l);
    }

    public List<StudentContext> getStudents() {
        return Collections.unmodifiableList(students);
    }

    private void addStudent(StudentContext s) {
        students.add(s);
        id_students.put(s.getStudent().id, s);
        for (ContextListener listener : listeners) listener.studentAdded(s);
    }

    private void removeStudent(StudentContext s) {
        students.remove(s);
        id_students.remove(s.getStudent().id);
        for (ContextListener listener : listeners) listener.studentRemoved(s);
    }

    @SuppressLint("StaticFieldLeak")
    public boolean login(@NonNull final Context ctx, @NonNull final String email, @NonNull final String password) throws ExecutionException, InterruptedException {
        return new AsyncTask<String, Integer, Boolean>() {
            @Override
            protected Boolean doInBackground(String... strings) {
                try {
                    Response<User> response = resource.login(strings[0], strings[1]).execute();
                    if (!response.isSuccessful()) return false;
                    Log.i(TAG, "Login successful..");
                    User user = response.body();

                    // we set the parameters of init's user (these parameters will be communicated to the server..)
                    user.par_types = get_par_types(ctx);
                    user.par_values = get_par_values(ctx);

                    setUser(ctx, user);

                    // we add the teachers and the following lessons..
                    for (Follow follow : user.follows.values()) {
                        if (id_teachers.get(follow.lesson.teacher.user.id) == null)
                            addTeacher(new TeacherContext(follow.lesson.teacher.user));
                        addFollowingLesson(new FollowingLessonContext(follow.lesson));
                    }

                    // we add the students and the teaching lessons..
                    for (Teach teach : user.teachs.values()) {
                        for (Follow follow : teach.lesson.students.values())
                            if (id_students.get(follow.user.id) == null)
                                addStudent(new StudentContext(follow.user));
                        addTeachingLesson(new TeachingLessonContext(teach.lesson));
                    }

                    return true;
                } catch (final IOException e) {
                    Log.w(TAG, "Login failed..", e);
                    ((Activity) ctx).runOnUiThread(new Runnable() {
                        public void run() {
                            Toast.makeText(ctx, e.getLocalizedMessage(), Toast.LENGTH_LONG).show();
                        }
                    });
                    return false;
                }
            }
        }.execute(email, password).get();
    }

    @SuppressLint("StaticFieldLeak")
    public boolean new_user(@NonNull final Context ctx, @NonNull final String email, @NonNull final String password, @NonNull final String first_name, @NonNull final String last_name) throws ExecutionException, InterruptedException {
        return new AsyncTask<String, Integer, Boolean>() {
            @Override
            protected Boolean doInBackground(String... strings) {
                try {
                    Response<User> response = resource.new_user(strings[0], strings[1], strings[2], strings[3]).execute();
                    if (!response.isSuccessful()) return false;
                    Log.i(TAG, "Login successful..");
                    User user = response.body();

                    // we set the parameters of init's user (these parameters will be communicated to the server..)
                    user.par_types = get_par_types(ctx);
                    user.par_values = get_par_values(ctx);

                    setUser(ctx, user);

                    return true;
                } catch (final IOException e) {
                    Log.w(TAG, "Login failed..", e);
                    ((Activity) ctx).runOnUiThread(new Runnable() {
                        public void run() {
                            Toast.makeText(ctx, e.getLocalizedMessage(), Toast.LENGTH_LONG).show();
                        }
                    });
                    return false;
                }
            }
        }.execute(email, password, first_name, last_name).get();
    }

    private Map<String, Parameter> get_par_types(@NonNull final Context ctx) {
        Map<String, Parameter> c_par_types = new HashMap<>();

        if (ContextCompat.checkSelfPermission(ctx, Manifest.permission.ACCESS_FINE_LOCATION) == PackageManager.PERMISSION_GRANTED) {
            Parameter gps = new Parameter();
            gps.name = "GPS";
            gps.properties = new HashMap<>(2);
            gps.properties.put("latitude", "numeric");
            gps.properties.put("longitude", "numeric");
            c_par_types.put("GPS", gps);
        }
        return c_par_types;
    }

    private Map<String, Map<String, String>> get_par_values(@NonNull final Context ctx) {
        Map<String, Map<String, String>> c_par_values = new HashMap<>();

        if (ContextCompat.checkSelfPermission(ctx, Manifest.permission.ACCESS_FINE_LOCATION) == PackageManager.PERMISSION_GRANTED) {
            Map<String, String> gps_pos = new HashMap<>(2);
            final Location last_location = ((LocationManager) ctx.getSystemService(Context.LOCATION_SERVICE)).getLastKnownLocation(LocationManager.GPS_PROVIDER);
            gps_pos.put("latitude", Double.toString(last_location.getLatitude()));
            gps_pos.put("longitude", Double.toString(last_location.getLongitude()));
            c_par_values.put("GPS", gps_pos);
        }
        return c_par_values;
    }

    @Override
    public void onLocationChanged(Location location) {
        if (mqtt != null && mqtt.isConnected()) {
            Map<String, String> gps_pos = new HashMap<>(2);
            gps_pos.put("latitude", Double.toString(location.getLatitude()));
            gps_pos.put("longitude", Double.toString(location.getLongitude()));
            try {
                mqtt.publish(user.id + "/output/GPS", GSON.toJson(gps_pos).getBytes(), 1, true);
            } catch (MqttException e) {
                Log.w(TAG, "GPS update MQTT communication failed..", e);
            }
        }
    }

    @Override
    public void onStatusChanged(String provider, int status, Bundle extras) {
    }

    @Override
    public void onProviderEnabled(String provider) {
    }

    @Override
    public void onProviderDisabled(String provider) {
    }

    public void addListener(ContextListener l) {
        listeners.add(l);
    }

    public void removeListener(ContextListener l) {
        listeners.remove(l);
    }
}
