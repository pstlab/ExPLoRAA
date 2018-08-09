package it.cnr.istc.exploraa;

import android.Manifest;
import android.app.Notification;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.Service;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.SharedPreferences;
import android.content.pm.PackageManager;
import android.location.Location;
import android.location.LocationListener;
import android.location.LocationManager;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;
import android.os.Binder;
import android.os.Build;
import android.os.Bundle;
import android.os.IBinder;
import android.preference.PreferenceManager;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.support.v4.app.NotificationCompat;
import android.support.v4.content.ContextCompat;
import android.util.Log;
import android.util.LongSparseArray;
import android.widget.Toast;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

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
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import it.cnr.istc.exploraa.api.ExPLoRAA;
import it.cnr.istc.exploraa.api.Follow;
import it.cnr.istc.exploraa.api.Lesson;
import it.cnr.istc.exploraa.api.LessonModel;
import it.cnr.istc.exploraa.api.Message;
import it.cnr.istc.exploraa.api.Parameter;
import it.cnr.istc.exploraa.api.Teach;
import it.cnr.istc.exploraa.api.User;
import retrofit2.Call;
import retrofit2.Callback;
import retrofit2.Response;
import retrofit2.Retrofit;
import retrofit2.converter.gson.GsonConverterFactory;

public class ExPLoRAAService extends Service implements LocationListener {

    private static final String TAG = "ExPLoRAAService";
    public static final String LOGIN = "Login";
    public static final String USER_CREATION = "User creation";
    public static final String ADDED_STIMULUS = "Added stimulus";
    public static final String REMOVED_STIMULUS = "Removed stimulus";
    public static final String CLEARED_STIMULI = "Cleared stimuli";
    public static final String ADDED_FOLLOWING_LESSON = "Added following lesson";
    public static final String REMOVED_FOLLOWING_LESSON = "Removed following lesson";
    public static final String CLEARED_FOLLOWING_LESSONS = "Cleared following lessons";
    public static final String ADDED_TEACHER = "Added teacher";
    public static final String REMOVED_TEACHER = "Removed teacher";
    public static final String CLEARED_TEACHERS = "Cleared teachers";
    public static final String ADDED_TEACHING_LESSON = "Added teaching lesson";
    public static final String REMOVED_TEACHING_LESSON = "Removed teaching lesson";
    public static final String CLEARED_TEACHING_LESSONS = "Cleared teaching lessons";
    public static final String ADDED_STUDENT = "Added student";
    public static final String REMOVED_STUDENT = "Removed student";
    public static final String CLEARED_STUDENTS = "Cleared students";
    public static final Gson GSON = new GsonBuilder().registerTypeAdapter(Message.class, Message.ADAPTER).registerTypeAdapter(LessonModel.class, LessonModel.ADAPTER).create();
    private final IBinder binder = new ExPLoRAABinder();
    private ExPLoRAA resource;
    private MqttClient mqtt;
    /**
     * The current user.
     */
    private User user;
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
    private boolean logging_in = false;
    private BroadcastReceiver connection_receiver = new BroadcastReceiver() {
        @Override
        public void onReceive(Context context, Intent intent) {
            Log.i(TAG, "Received connection change event..");
            if (user == null) {
                NetworkInfo activeNetwork = ((ConnectivityManager) getSystemService(Context.CONNECTIVITY_SERVICE)).getActiveNetworkInfo();
                if (activeNetwork != null && activeNetwork.isConnected() && user == null) {
                    SharedPreferences shared_prefs = PreferenceManager.getDefaultSharedPreferences(ExPLoRAAService.this);
                    if (shared_prefs.contains(getString(R.string.email)) && shared_prefs.contains(getString(R.string.password)))
                        login(shared_prefs.getString(getString(R.string.email), null), shared_prefs.getString(getString(R.string.password), null));
                }
            }
        }
    };

    public static String convertTimeToString(long time) {
        long second = (time / 1000) % 60;
        long minute = (time / (1000 * 60)) % 60;
        long hour = (time / (1000 * 60 * 60)) % 24;
        long days = (time / (1000 * 60 * 60 * 24));
        if (days == 0) {
            if (hour == 0) {
                return String.format("%02d:%02d", minute, second);
            } else {
                return String.format("%02d:%02d:%02d", hour, minute, second);
            }
        } else {
            return String.format("%03d:%02d:%02d:%02d", days, hour, minute, second);
        }
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        return START_STICKY;
    }

    @Override
    public void onCreate() {
        super.onCreate();
        Log.i(TAG, "Creating ExPLoRAA service..");
        Retrofit retrofit = new Retrofit.Builder().baseUrl("http://" + BuildConfig.HOST + ":" + BuildConfig.SERVICE_PORT + "/ExPLoRAA/resources/").addConverterFactory(GsonConverterFactory.create(GSON)).build();
        resource = retrofit.create(ExPLoRAA.class);

        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            NotificationManager nm = (NotificationManager) getSystemService(NOTIFICATION_SERVICE);
            nm.createNotificationChannel(new NotificationChannel(getString(R.string.app_name), getString(R.string.app_name), NotificationManager.IMPORTANCE_DEFAULT));
        }

        registerReceiver(connection_receiver, new IntentFilter(ConnectivityManager.CONNECTIVITY_ACTION));

        NetworkInfo activeNetwork = ((ConnectivityManager) getSystemService(Context.CONNECTIVITY_SERVICE)).getActiveNetworkInfo();
        if (activeNetwork != null && activeNetwork.isConnected()) {
            SharedPreferences shared_prefs = PreferenceManager.getDefaultSharedPreferences(this);
            if (shared_prefs.contains(getString(R.string.email)) && shared_prefs.contains(getString(R.string.password)))
                login(shared_prefs.getString(getString(R.string.email), null), shared_prefs.getString(getString(R.string.password), null));
        }

        final Notification notification = new NotificationCompat.Builder(this, getString(R.string.app_name))
                .setSmallIcon(R.drawable.ic_backpacker)
                .setContentTitle(getString(R.string.app_name))
                .setContentText("ExPLoRAA is running..")
                .build();
        startForeground(1, notification);
    }

    @Override
    public boolean onUnbind(Intent intent) {
        return super.onUnbind(intent);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        Log.i(TAG, "Destroying ExPLoRAA service..");
        if (user != null)
            logout();
        if (mqtt != null && mqtt.isConnected()) try {
            mqtt.disconnect();
            mqtt.close();
        } catch (MqttException e) {
            Log.e(TAG, null, e);
        }
        unregisterReceiver(connection_receiver);
    }

    @Nullable
    @Override
    public IBinder onBind(Intent intent) {
        return binder;
    }

    /**
     * Returns the current user.
     *
     * @return the current user.
     */
    public User getUser() {
        return user;
    }

    /**
     * Sets the current user.
     *
     * @param user the current user.
     */
    private void setUser(@NonNull final User user) {
        if (this.user != user) {
            if (this.user != null) try {
                // we clear the current data..
                par_vals.clear();
                // a user might become null as a consequence of a connection loss..
                // we broadcast the lost of a parameter..
                if (mqtt.isConnected()) for (Parameter par : par_types)
                    mqtt.publish(this.user.id + "/output", GSON.toJson(new Message.RemoveParameter(par.name)).getBytes(), 1, false);
                if (ContextCompat.checkSelfPermission(this, Manifest.permission.ACCESS_FINE_LOCATION) == PackageManager.PERMISSION_GRANTED)
                    ((LocationManager) getSystemService(Context.LOCATION_SERVICE)).removeUpdates(this);
                par_types.clear();
                id_par_types.clear();

                // we clear the stimuli..
                stimuli.clear();
                sendBroadcast(new Intent(CLEARED_STIMULI));

                // we clear the following lessons..
                if (mqtt.isConnected()) for (FollowingLessonContext l_ctx : following_lessons) {
                    // we unsubscribe from the lesson's time and state..
                    mqtt.unsubscribe(this.user.id + "/input/lesson-" + l_ctx.getLesson().id + "/time");
                    mqtt.unsubscribe(this.user.id + "/input/lesson-" + l_ctx.getLesson().id + "/state");
                }
                following_lessons.clear();
                id_following_lessons.clear();
                sendBroadcast(new Intent(CLEARED_FOLLOWING_LESSONS));

                // we clear the teachers..
                if (mqtt.isConnected()) for (TeacherContext s_ctx : teachers) {
                    mqtt.unsubscribe(s_ctx.getTeacher().id + "/output/on-line");
                }
                teachers.clear();
                id_teachers.clear();
                sendBroadcast(new Intent(CLEARED_TEACHERS));

                models.clear();

                // we clear the teaching lessons..
                if (mqtt.isConnected()) for (TeachingLessonContext l_ctx : teaching_lessons) {
                    // we unsubscribe from the lesson's time and state..
                    mqtt.unsubscribe(this.user.id + "/input/lesson-" + l_ctx.getLesson().id + "/time");
                    mqtt.unsubscribe(this.user.id + "/input/lesson-" + l_ctx.getLesson().id + "/state");
                }
                teaching_lessons.clear();
                id_teaching_lessons.clear();
                sendBroadcast(new Intent(CLEARED_TEACHING_LESSONS));

                // we clear the students..
                if (mqtt.isConnected()) for (StudentContext s_ctx : students) {
                    mqtt.unsubscribe(s_ctx.getStudent().id + "/output/on-line");
                }
                students.clear();
                id_students.clear();
                sendBroadcast(new Intent(CLEARED_STUDENTS));

                if (mqtt.isConnected()) mqtt.disconnect();
                mqtt.close();
                Log.i(TAG, "Disconnected from the MQTT broker..");
                mqtt = null;
            } catch (MqttException ex) {
                Log.e(TAG, null, ex);
            }

            if (user != null) try {
                // we create a new MQTT connection..
                mqtt = new MqttClient("tcp://" + BuildConfig.HOST + ":" + BuildConfig.MQTT_PORT, String.valueOf(user.id), new MemoryPersistence());
                mqtt.setCallback(new MqttCallback() {
                    @Override
                    public void connectionLost(Throwable cause) {
                        Log.e(TAG, "Connection lost..", cause);
                        logout();
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

                // we subscribe to the input topic of the user..
                mqtt.subscribe(user.id + "/input", new IMqttMessageListener() {
                    @Override
                    public void messageArrived(String topic, MqttMessage message) {
                        Log.d(TAG, "Message arrived: " + topic + " - " + message);
                        Message m = GSON.fromJson(new String(message.getPayload()), Message.class);
                        switch (m.message_type) {
                            case RemoveLesson:
                                // a teacher has removed a lesson for this student..
                                final Message.RemoveLesson lost_lesson = (Message.RemoveLesson) m;
                                removeFollowingLesson(id_following_lessons.get(lost_lesson.lesson));
                                break;
                            case FollowLesson:
                                // a new student is following a lesson of this teacher..
                                final Message.FollowLesson follow_lesson = (Message.FollowLesson) m;
                                id_teaching_lessons.get(follow_lesson.lesson).addStudent(new StudentContext(ExPLoRAAService.this, follow_lesson.student));
                                break;
                            case UnfollowLesson:
                                // a student is not following a lesson of this user anymore..
                                final Message.UnfollowLesson unfollow_lesson = (Message.UnfollowLesson) m;
                                id_teaching_lessons.get(unfollow_lesson.lesson).removeStudent(id_students.get(unfollow_lesson.student));
                                break;
                            case Token:
                                // a new token has been created for a teaching lesson..
                                final Message.Token token = (Message.Token) m;
                                id_teaching_lessons.get(token.lesson_id).addToken(token);
                                break;
                            case TokenUpdate:
                                // a token of a teaching lesson has been updated..
                                final Message.TokenUpdate token_update = (Message.TokenUpdate) m;
                                id_teaching_lessons.get(token_update.lesson_id).updateToken(token_update.id, token_update.time, token_update.min != null ? token_update.min : Long.MIN_VALUE, token_update.max != null ? token_update.max : Long.MAX_VALUE);
                                break;
                            case RemoveToken:
                                // a token of a teaching lesson has been removed..
                                final Message.RemoveToken remove_token = (Message.RemoveToken) m;
                                id_teaching_lessons.get(remove_token.lesson_id).removeToken(id_teaching_lessons.get(remove_token.lesson_id).getToken(remove_token.id).getToken());
                                break;
                            case Stimulus:
                                // a new stimulus has been created for a following lesson..
                                final Message.Stimulus stimulus = (Message.Stimulus) m;
                                id_following_lessons.get(stimulus.lesson_id).addStimulus(stimulus);
                                break;
                            case RemoveStimulus:
                                // a stimulus has been removed for a following lesson..
                                final Message.RemoveStimulus hide_stimulus = (Message.RemoveStimulus) m;
                                Message.Stimulus s = null;
                                for (Message.Stimulus c_s : id_following_lessons.get(hide_stimulus.lesson_id).getStimuli())
                                    if (c_s.id == hide_stimulus.id) {
                                        s = c_s;
                                        break;
                                    }
                                id_following_lessons.get(hide_stimulus.lesson_id).removeStimulus(s);
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
                if (ContextCompat.checkSelfPermission(this, Manifest.permission.ACCESS_FINE_LOCATION) == PackageManager.PERMISSION_GRANTED)
                    ((LocationManager) getSystemService(Context.LOCATION_SERVICE)).requestLocationUpdates(LocationManager.GPS_PROVIDER, 0, 0, this);

                for (Map.Entry<String, Map<String, String>> par_val : user.par_values.entrySet()) {
                    par_vals.put(par_val.getKey(), par_val.getValue());
                    // we broadcast the the new value of the parameter..
                    mqtt.publish(user.id + "/output/" + par_val.getKey(), GSON.toJson(par_val.getValue()).getBytes(), 1, true);
                }
            } catch (MqttException e) {
                Log.w(TAG, "MQTT Connection failed..", e);
            }
            this.user = user;
        }
    }

    void addStimulus(@NonNull final Message.Stimulus stimulus) {
        final int pos = stimuli.size();
        stimuli.add(stimulus);
        Intent added_stimulus_intent = new Intent(ADDED_STIMULUS);
        added_stimulus_intent.putExtra("position", pos);
        sendBroadcast(added_stimulus_intent);
    }

    void removeStimulus(@NonNull final Message.Stimulus stimulus) {
        final int pos = stimuli.indexOf(stimulus);
        stimuli.remove(pos);
        Intent removed_stimulus_intent = new Intent(REMOVED_STIMULUS);
        removed_stimulus_intent.putExtra("position", pos);
        sendBroadcast(removed_stimulus_intent);
    }

    public List<Message.Stimulus> getStimuli() {
        return Collections.unmodifiableList(stimuli);
    }

    private void addFollowingLesson(@NonNull final FollowingLessonContext l_ctx) {
        final int pos = following_lessons.size();
        following_lessons.add(l_ctx);
        id_following_lessons.put(l_ctx.getLesson().id, l_ctx);
        if (id_teachers.get(l_ctx.getLesson().teacher.user.id) == null)
            addTeacher(new TeacherContext(ExPLoRAAService.this, l_ctx.getLesson().teacher.user));
        try {
            // we subscribe to the lesson's time..
            mqtt.subscribe(l_ctx.getLesson().teacher.user.id + "/input/lesson-" + l_ctx.getLesson().id + "/time", new IMqttMessageListener() {
                @Override
                public void messageArrived(final String topic, final MqttMessage message) {
                    l_ctx.setTime(Long.parseLong(new String(message.getPayload())));
                }
            });
            // we subscribe to the lesson's state..
            mqtt.subscribe(l_ctx.getLesson().teacher.user.id + "/input/lesson-" + l_ctx.getLesson().id + "/state", new IMqttMessageListener() {
                @Override
                public void messageArrived(final String topic, final MqttMessage message) {
                    l_ctx.setState(Lesson.LessonState.valueOf(new String(message.getPayload())));
                }
            });
        } catch (MqttException ex) {
            Log.w(TAG, "MQTT subscription failed..", ex);
        }
        Intent added_following_lesson_intent = new Intent(ADDED_FOLLOWING_LESSON);
        added_following_lesson_intent.putExtra("position", pos);
        sendBroadcast(added_following_lesson_intent);
    }

    public FollowingLessonContext getFollowingLesson(final long id) {
        return id_following_lessons.get(id);
    }

    public List<FollowingLessonContext> getFollowingLessons() {
        return Collections.unmodifiableList(following_lessons);
    }

    private void removeFollowingLesson(@NonNull final FollowingLessonContext l_ctx) {
        // we remove the lesson's stimuli..
        for (Message.Stimulus st : l_ctx.getStimuli()) removeStimulus(st);
        int pos = following_lessons.indexOf(l_ctx);
        following_lessons.remove(pos);
        id_following_lessons.remove(l_ctx.getLesson().id);
        if (mqtt.isConnected()) {
            try {
                // we unsubscribe from the lesson's time and state..
                mqtt.unsubscribe(l_ctx.getLesson().teacher.user.id + "/input/lesson-" + l_ctx.getLesson().id + "/time");
                mqtt.unsubscribe(l_ctx.getLesson().teacher.user.id + "/input/lesson-" + l_ctx.getLesson().id + "/state");
            } catch (MqttException ex) {
                Log.w(TAG, "MQTT unsubscription failed..", ex);
            }
        }
        Set<Long> c_teachers = new HashSet<>();
        for (FollowingLessonContext l : following_lessons)
            c_teachers.add(l.getLesson().teacher.user.id);
        Set<Long> to_remove_teachers = new HashSet<>();
        for (TeacherContext t_ctx : teachers)
            if (!c_teachers.contains(t_ctx.getTeacher().id))
                to_remove_teachers.add(t_ctx.getTeacher().id);
        for (Long to_remove_teacher : to_remove_teachers)
            removeTeacher(id_teachers.get(to_remove_teacher));
        Intent removed_following_lesson_intent = new Intent(REMOVED_FOLLOWING_LESSON);
        removed_following_lesson_intent.putExtra("position", pos);
        sendBroadcast(removed_following_lesson_intent);
    }

    private void addTeacher(@NonNull final TeacherContext t_ctx) {
        final int pos = teachers.size();
        teachers.add(t_ctx);
        id_teachers.put(t_ctx.getTeacher().id, t_ctx);
        try {
            mqtt.subscribe(t_ctx.getTeacher().id + "/output/on-line", new IMqttMessageListener() {
                @Override
                public void messageArrived(String topic, MqttMessage message) {
                    t_ctx.setOnLine(Boolean.parseBoolean(new String(message.getPayload())));
                }
            });
        } catch (MqttException ex) {
            Log.w(TAG, "MQTT subscription failed..", ex);
        }
        Intent added_teacher_intent = new Intent(ADDED_TEACHER);
        added_teacher_intent.putExtra("position", pos);
        sendBroadcast(added_teacher_intent);
    }

    private void removeTeacher(@NonNull final TeacherContext t_ctx) {
        int pos = teachers.indexOf(t_ctx);
        teachers.remove(pos);
        id_teachers.remove(t_ctx.getTeacher().id);
        if (mqtt.isConnected()) try {
            // we might be removing teachers as a consequence of a connection loss..
            mqtt.unsubscribe(t_ctx.getTeacher().id + "/output/on-line");
        } catch (MqttException ex) {
            Log.w(TAG, "MQTT unsubscription failed..", ex);
        }
        Intent removed_teacher_intent = new Intent(REMOVED_TEACHER);
        removed_teacher_intent.putExtra("position", pos);
        sendBroadcast(removed_teacher_intent);
    }

    public TeacherContext getTeacher(final long id) {
        return id_teachers.get(id);
    }

    public List<TeacherContext> getTeachers() {
        return Collections.unmodifiableList(teachers);
    }

    public List<LessonModel> getModels() {
        return Collections.unmodifiableList(models);
    }

    void addModel(@NonNull final LessonModel model) {
        models.add(model);
    }

    private void addTeachingLesson(@NonNull final TeachingLessonContext l_ctx) {
        final int pos = teaching_lessons.size();
        teaching_lessons.add(l_ctx);
        id_teaching_lessons.put(l_ctx.getLesson().id, l_ctx);
        if (l_ctx.getLesson().students != null)
            for (Follow follow : l_ctx.getLesson().students.values())
                l_ctx.addStudent(id_students.get(follow.user.id) != null ? id_students.get(follow.user.id) : new StudentContext(this, follow.user));
        try {
            // we subscribe to the lesson's time..
            mqtt.subscribe(user.id + "/input/lesson-" + l_ctx.getLesson().id + "/time", new IMqttMessageListener() {
                @Override
                public void messageArrived(final String topic, final MqttMessage message) {
                    l_ctx.setTime(Long.parseLong(new String(message.getPayload())));
                }
            });
            // we subscribe to the lesson's state..
            mqtt.subscribe(user.id + "/input/lesson-" + l_ctx.getLesson().id + "/state", new IMqttMessageListener() {
                @Override
                public void messageArrived(final String topic, final MqttMessage message) {
                    l_ctx.setState(Lesson.LessonState.valueOf(new String(message.getPayload())));
                }
            });
        } catch (MqttException ex) {
            Log.w(TAG, "MQTT subscription failed..", ex);
        }
        Intent added_teaching_lesson_intent = new Intent(ADDED_TEACHING_LESSON);
        added_teaching_lesson_intent.putExtra("position", pos);
        sendBroadcast(added_teaching_lesson_intent);
    }

    public TeachingLessonContext getTeachingLesson(final long id) {
        return id_teaching_lessons.get(id);
    }

    public List<TeachingLessonContext> getTeachingLessons() {
        return Collections.unmodifiableList(teaching_lessons);
    }

    private void removeTeachingLesson(@NonNull final TeachingLessonContext l_ctx) {
        int pos = teaching_lessons.indexOf(l_ctx);
        teaching_lessons.remove(pos);
        id_teaching_lessons.remove(l_ctx.getLesson().id);
        if (user != null && mqtt.isConnected()) try {
            // we unsubscribe from the lesson's time and state..
            mqtt.unsubscribe(user.id + "/input/lesson-" + l_ctx.getLesson().id + "/time");
            mqtt.unsubscribe(user.id + "/input/lesson-" + l_ctx.getLesson().id + "/state");
        } catch (MqttException ex) {
            Log.w(TAG, "MQTT unsubscription failed..", ex);
        }
        Set<Long> c_students = new HashSet<>();
        for (TeachingLessonContext l : teaching_lessons)
            for (StudentContext student : l.getStudents()) c_students.add(student.getStudent().id);
        Set<Long> to_remove_students = new HashSet<>();
        for (StudentContext s_ctx : students)
            if (!c_students.contains(s_ctx.getStudent().id))
                to_remove_students.add(s_ctx.getStudent().id);
        for (Long to_remove_student : to_remove_students)
            removeStudent(id_students.get(to_remove_student));
        Intent removed_teaching_lesson_intent = new Intent(REMOVED_TEACHING_LESSON);
        removed_teaching_lesson_intent.putExtra("position", pos);
        sendBroadcast(removed_teaching_lesson_intent);
    }

    void addStudent(@NonNull final StudentContext s_ctx) {
        final int pos = teachers.size();
        students.add(s_ctx);
        id_students.put(s_ctx.getStudent().id, s_ctx);
        try {
            // we subscribe to be notified whether the student gets online/offline..
            mqtt.subscribe(s_ctx.getStudent().id + "/output/on-line", new IMqttMessageListener() {
                @Override
                public void messageArrived(final String topic, final MqttMessage message) {
                    s_ctx.setOnLine(Boolean.parseBoolean(new String(message.getPayload())));
                }
            });

            // TODO: register to the student's parameters..
        } catch (MqttException ex) {
            Log.w(TAG, "MQTT subscription failed..", ex);
        }
        Intent added_student_intent = new Intent(ADDED_STUDENT);
        added_student_intent.putExtra("position", pos);
        sendBroadcast(added_student_intent);
    }

    void removeStudent(@NonNull final StudentContext s_ctx) {
        int pos = students.indexOf(s_ctx);
        students.remove(pos);
        id_students.remove(s_ctx.getStudent().id);
        try {
            if (mqtt.isConnected()) {
                mqtt.unsubscribe(s_ctx.getStudent().id + "/output/on-line");
            }
        } catch (MqttException ex) {
            Log.w(TAG, "MQTT unsubscription failed..", ex);
        }
        Intent removed_student_intent = new Intent(REMOVED_STUDENT);
        removed_student_intent.putExtra("position", pos);
        sendBroadcast(removed_student_intent);
    }

    public StudentContext getStudent(final long id) {
        return id_students.get(id);
    }

    public List<StudentContext> getStudents() {
        return Collections.unmodifiableList(students);
    }

    /**
     * Creates a new user.
     *
     * @param email      the email of the new user.
     * @param password   the password of the new user.
     * @param first_name the first name of the new user.
     * @param last_name  the last name of the new user.
     */
    public void new_user(@NonNull final String email, @NonNull final String password, @NonNull final String first_name, @NonNull final String last_name) {
        assert user == null;
        Log.i(TAG, "Creating new user..");
        resource.new_user(email, password, first_name, last_name).enqueue(new Callback<User>() {
            @Override
            public void onResponse(Call<User> call, Response<User> response) {
                Intent user_creation_intent = new Intent(USER_CREATION);
                user_creation_intent.putExtra("successful", response.isSuccessful());
                sendBroadcast(user_creation_intent);

                if (response.isSuccessful()) {
                    Log.i(TAG, "User creation successful..");
                    final User user = response.body();

                    // we set the parameters of init's user (these parameters will be communicated to the server..)
                    assert user != null;
                    user.par_types = get_par_types();
                    user.par_values = get_par_values();

                    setUser(user);
                } else try {
                    Toast.makeText(ExPLoRAAService.this, response.errorBody().string(), Toast.LENGTH_SHORT).show();
                } catch (IOException e) {
                    Log.e(TAG, null, e);
                }
            }

            @Override
            public void onFailure(Call<User> call, Throwable t) {
                Log.e(TAG, "User creation failed..", t);
            }
        });
    }

    /**
     * Logs into the ExPLoRAA service, initializing stimuli, followed lessons, teachers, teached lessons and students.
     *
     * @param email    the email which identifies the user.
     * @param password the password of the user.
     */
    public void login(String email, String password) {
        assert user == null;
        if (logging_in) return;
        logging_in = true;
        Log.i(TAG, "Logging in..");
        resource.login(email, password).enqueue(new Callback<User>() {
            @Override
            public void onResponse(Call<User> call, Response<User> response) {
                Intent login_intent = new Intent(LOGIN);
                login_intent.putExtra("successful", response.isSuccessful());
                sendBroadcast(login_intent);

                if (response.isSuccessful()) {
                    Log.i(TAG, "Login successful..");
                    final User user = response.body();

                    // we set the parameters of init's user (these parameters will be communicated to the server..)
                    assert user != null;
                    user.par_types = get_par_types();
                    user.par_values = get_par_values();

                    setUser(user);

                    // we add the following lessons..
                    for (Follow follow : user.follows.values())
                        addFollowingLesson(new FollowingLessonContext(ExPLoRAAService.this, follow.lesson));

                    // we add the teaching lessons..
                    for (Teach teach : user.teachs.values())
                        addTeachingLesson(new TeachingLessonContext(ExPLoRAAService.this, teach.lesson));

                    // we add the available lesson models..
                    models.addAll(user.models.values());
                } else try {
                    Toast.makeText(ExPLoRAAService.this, response.errorBody().string(), Toast.LENGTH_SHORT).show();
                } catch (IOException e) {
                    Log.e(TAG, null, e);
                }
                logging_in = false;
            }

            @Override
            public void onFailure(Call<User> call, Throwable t) {
                Log.e(TAG, "Login failed..", t);
                logging_in = false;
            }
        });
    }

    /**
     * Logs out from the ExPLoRAA service, clearing stimuli, followed lessons, teachers, teached lessons and students.
     */
    public void logout() {
        assert user != null;
        Log.i(TAG, "Logging out current user..");
        setUser(null);
    }

    /**
     * Retrieves from the ExLOoRAA service all the current lessons.
     *
     * @param callback a callback for managing the request.
     */
    public void get_lessons(Callback<Collection<Lesson>> callback) {
        Log.i(TAG, "Retrieving all the lessons..");
        resource.get_lessons().enqueue(callback);
    }

    /**
     * Creates a new request for following an existing lesson.
     *
     * @param lesson    the lesson the current user wants to folow.
     * @param interests the interests about the lesson.
     */
    public void follow_lesson(@NonNull final Lesson lesson, @NonNull final ArrayList<CharSequence> interests) {
        Log.i(TAG, "Following lesson " + lesson.id + "..");
        resource.follow(user.id, lesson.id, GSON.toJson(interests)).enqueue(new Callback<Lesson>() {
            @Override
            public void onResponse(Call<Lesson> call, Response<Lesson> response) {
                if (!response.isSuccessful()) try {
                    Toast.makeText(ExPLoRAAService.this, response.errorBody().string(), Toast.LENGTH_SHORT).show();
                } catch (IOException e) {
                    e.printStackTrace();
                }
                else
                    addFollowingLesson(new FollowingLessonContext(ExPLoRAAService.this, response.body()));
            }

            @Override
            public void onFailure(Call<Lesson> call, Throwable t) {
                Log.e(TAG, null, t);
            }
        });
    }

    /**
     * Creates a new request for unfollowing a following lesson.
     *
     * @param l_ctx the lesson context representing the lesson that the current user does not want to follow anymore.
     */
    public void unfollow_lesson(@NonNull final FollowingLessonContext l_ctx) {
        Log.i(TAG, "Unfollowing lesson " + l_ctx.getLesson().id + "..");
        resource.unfollow(user.id, l_ctx.getLesson().id).enqueue(new Callback<Void>() {
            @Override
            public void onResponse(Call<Void> call, Response<Void> response) {
                if (!response.isSuccessful()) try {
                    Toast.makeText(ExPLoRAAService.this, response.errorBody().string(), Toast.LENGTH_SHORT).show();
                } catch (IOException e) {
                    e.printStackTrace();
                }
                else
                    removeFollowingLesson(l_ctx);
            }

            @Override
            public void onFailure(Call<Void> call, Throwable t) {
                Log.e(TAG, null, t);
            }
        });
    }

    /**
     * Creates and adds a new teaching lesson with the given name starting from the given model.
     *
     * @param name  the name of the new lesson.
     * @param model the model of the new lesson.
     */
    public void add_teaching_lesson(@NonNull final String name, @NonNull final LessonModel model) {
        Log.i(TAG, "Creating a new lesson..");
        (model.id == null ? resource.new_lesson(user.id, name, GSON.toJson(model)) : resource.new_lesson(user.id, name, model.id)).enqueue(new Callback<Lesson>() {
            @Override
            public void onResponse(Call<Lesson> call, Response<Lesson> response) {
                if (!response.isSuccessful()) try {
                    Toast.makeText(ExPLoRAAService.this, response.errorBody().string(), Toast.LENGTH_SHORT).show();
                } catch (IOException e) {
                    e.printStackTrace();
                }
                else {
                    final Lesson l = response.body();
                    // we set the model of the returned lesson..
                    l.model = model;
                    // the new lesson has not any tokens yet..
                    l.tokens = new ArrayList<>();
                    // we add a new context for the returned lesson..
                    addTeachingLesson(new TeachingLessonContext(ExPLoRAAService.this, l));

                    // we solve the lesson..
                    resource.solve(l.id).enqueue(new Callback<Void>() {
                        @Override
                        public void onResponse(Call<Void> call, Response<Void> response) {
                            if (!response.isSuccessful()) try {
                                Toast.makeText(ExPLoRAAService.this, response.errorBody().string(), Toast.LENGTH_SHORT).show();
                            } catch (IOException e) {
                                e.printStackTrace();
                            }
                        }

                        @Override
                        public void onFailure(Call<Void> call, Throwable t) {
                            Log.e(TAG, null, t);
                        }
                    });
                }
            }

            @Override
            public void onFailure(Call<Lesson> call, Throwable t) {
                Log.e(TAG, null, t);
            }
        });
    }

    /**
     * Removes a lesson which is currently teached by this user.
     *
     * @param l_ctx the teaching lesson to remove.
     */
    public void remove_teaching_lesson(@NonNull final TeachingLessonContext l_ctx) {
        Log.i(TAG, "Deleting lesson " + l_ctx.getLesson().id + "..");
        resource.delete_lesson(l_ctx.getLesson().id).enqueue(new Callback<Void>() {
            @Override
            public void onResponse(Call<Void> call, Response<Void> response) {
                if (!response.isSuccessful()) try {
                    Toast.makeText(ExPLoRAAService.this, response.errorBody().string(), Toast.LENGTH_SHORT).show();
                } catch (IOException e) {
                    e.printStackTrace();
                }
                else
                    removeTeachingLesson(l_ctx);
            }

            @Override
            public void onFailure(Call<Void> call, Throwable t) {
                Log.e(TAG, null, t);
            }
        });
    }

    /**
     * Starts the execution of the given lesson.
     *
     * @param lesson the lesson to play.
     */
    public void play(Lesson lesson) {
        Log.i(TAG, "Playing lesson " + lesson.id + "..");
        resource.play(lesson.id).enqueue(new Callback<Void>() {
            @Override
            public void onResponse(Call<Void> call, Response<Void> response) {
                if (!response.isSuccessful()) try {
                    Toast.makeText(ExPLoRAAService.this, response.errorBody().string(), Toast.LENGTH_SHORT).show();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }

            @Override
            public void onFailure(Call<Void> call, Throwable t) {
                Log.e(TAG, null, t);
            }
        });
    }

    /**
     * Pauses the execution of the given lesson.
     *
     * @param lesson the lesson to pause.
     */
    public void pause(Lesson lesson) {
        Log.i(TAG, "Pausing lesson " + lesson.id + "..");
        resource.pause(lesson.id).enqueue(new Callback<Void>() {
            @Override
            public void onResponse(Call<Void> call, Response<Void> response) {
                if (!response.isSuccessful()) try {
                    Toast.makeText(ExPLoRAAService.this, response.errorBody().string(), Toast.LENGTH_SHORT).show();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }

            @Override
            public void onFailure(Call<Void> call, Throwable t) {
                Log.e(TAG, null, t);
            }
        });
    }

    /**
     * Stops the execution of the given lesson.
     *
     * @param lesson the lesson to stop.
     */
    public void stop(Lesson lesson) {
        Log.i(TAG, "Stopping lesson " + lesson.id + "..");
        resource.stop(lesson.id).enqueue(new Callback<Void>() {
            @Override
            public void onResponse(Call<Void> call, Response<Void> response) {
                if (!response.isSuccessful()) try {
                    Toast.makeText(ExPLoRAAService.this, response.errorBody().string(), Toast.LENGTH_SHORT).show();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }

            @Override
            public void onFailure(Call<Void> call, Throwable t) {
                Log.e(TAG, null, t);
            }
        });
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

    private Map<String, Parameter> get_par_types() {
        Map<String, Parameter> c_par_types = new HashMap<>();

        if (ContextCompat.checkSelfPermission(this, Manifest.permission.ACCESS_FINE_LOCATION) == PackageManager.PERMISSION_GRANTED) {
            Parameter gps = new Parameter();
            gps.name = "GPS";
            gps.properties = new HashMap<>(2);
            gps.properties.put("latitude", "numeric");
            gps.properties.put("longitude", "numeric");
            c_par_types.put("GPS", gps);
        }
        return c_par_types;
    }

    public class ExPLoRAABinder extends Binder {

        public ExPLoRAAService getService() {
            return ExPLoRAAService.this;
        }
    }

    private Map<String, Map<String, String>> get_par_values() {
        Map<String, Map<String, String>> c_par_values = new HashMap<>();

        if (ContextCompat.checkSelfPermission(this, Manifest.permission.ACCESS_FINE_LOCATION) == PackageManager.PERMISSION_GRANTED) {
            Map<String, String> gps_pos = new HashMap<>(2);
            final Location last_location = ((LocationManager) getSystemService(Context.LOCATION_SERVICE)).getLastKnownLocation(LocationManager.GPS_PROVIDER);
            if (last_location != null) {
                gps_pos.put("latitude", Double.toString(last_location.getLatitude()));
                gps_pos.put("longitude", Double.toString(last_location.getLongitude()));
            } else {
                gps_pos.put("latitude", Double.toString(0));
                gps_pos.put("longitude", Double.toString(0));
            }
            c_par_values.put("GPS", gps_pos);
        }
        return c_par_values;
    }
}
