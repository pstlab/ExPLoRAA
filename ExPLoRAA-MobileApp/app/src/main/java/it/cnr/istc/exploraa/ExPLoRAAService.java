package it.cnr.istc.exploraa;

import android.Manifest;
import android.app.Notification;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.app.Service;
import android.app.TaskStackBuilder;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.SharedPreferences;
import android.content.pm.PackageManager;
import android.hardware.Sensor;
import android.hardware.SensorEvent;
import android.hardware.SensorEventListener;
import android.hardware.SensorManager;
import android.location.Location;
import android.location.LocationListener;
import android.location.LocationManager;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;
import android.os.Binder;
import android.os.Build;
import android.os.Bundle;
import android.os.Handler;
import android.os.IBinder;
import android.preference.PreferenceManager;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.support.v4.app.NotificationCompat;
import android.support.v4.app.NotificationManagerCompat;
import android.support.v4.content.ContextCompat;
import android.util.Log;
import android.util.LongSparseArray;
import android.widget.Toast;

import com.empatica.empalink.ConnectionNotAllowedException;
import com.empatica.empalink.EmpaDeviceManager;
import com.empatica.empalink.EmpaticaDevice;
import com.empatica.empalink.config.EmpaSensorType;
import com.empatica.empalink.config.EmpaStatus;
import com.empatica.empalink.delegate.EmpaDataDelegate;
import com.empatica.empalink.delegate.EmpaStatusDelegate;
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
import java.util.Timer;
import java.util.TimerTask;

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

public class ExPLoRAAService extends Service implements LocationListener, SensorEventListener, EmpaDataDelegate, EmpaStatusDelegate {

    private static final String TAG = "ExPLoRAAService";
    private static final int LOCATION_TIME = 1000 * 60 * 2; // two minutes..
    private static final int STEPS_TIME = 1000 * 10; // ten seconds..
    public static final String LOGIN = "Login";
    public static final String USER_CREATION = "User creation";
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
    private final Map<String, Parameter> id_par_types = new HashMap<>();
    /**
     * The current user's parameter values.
     */
    private final Map<String, Map<String, String>> par_vals = new HashMap<>();
    private Location current_location;
    private SensorManager sensor_manager;
    private Sensor step_detector;
    private long last_step_timestamp = 0;
    private Timer steps_timer;
    private EmpaDeviceManager device_manager;
    /**
     * The received stimuli.
     */
    private final List<Message.Stimulus> stimuli = new ArrayList<>();
    /**
     * The lessons followed as a student.
     */
    private final List<FollowingLessonContext> following_lessons = new ArrayList<>();
    private final LongSparseArray<FollowingLessonContext> id_following_lessons = new LongSparseArray<>();
    private final LongSparseArray<FollowingLessonContext.FollowingLessonListener> id_following_lesson_listener = new LongSparseArray<>();
    /**
     * The followed teachers.
     */
    private final List<TeacherContext> teachers = new ArrayList<>();
    private final LongSparseArray<TeacherContext> id_teachers = new LongSparseArray<>();
    private final LongSparseArray<TeacherContext.TeacherListener> id_teacher_listener = new LongSparseArray<>();
    /**
     * The lesson models associated to the teacher.
     */
    private final List<LessonModel> models = new ArrayList<>();
    /**
     * The lessons followed as a teacher.
     */
    private final List<TeachingLessonContext> teaching_lessons = new ArrayList<>();
    private final LongSparseArray<TeachingLessonContext> id_teaching_lessons = new LongSparseArray<>();
    private final LongSparseArray<TeachingLessonContext.TeachingLessonListener> id_teaching_lesson_listener = new LongSparseArray<>();
    /**
     * The following students.
     */
    private final List<StudentContext> students = new ArrayList<>();
    private final LongSparseArray<StudentContext> id_students = new LongSparseArray<>();
    private final LongSparseArray<StudentContext.StudentListener> id_student_listener = new LongSparseArray<>();
    private final Collection<EmpaticaE4Listener> empatica_e4_listeners = new ArrayList<>();
    private final Collection<StimuliListener> stimuli_listeners = new ArrayList<>();
    private final Collection<FollowingLessonsListener> following_lessons_listeners = new ArrayList<>();
    private final Collection<TeachersListener> teachers_listeners = new ArrayList<>();
    private final Collection<TeachingLessonsListener> teaching_lessons_listeners = new ArrayList<>();
    private final Collection<StudentsListener> students_listeners = new ArrayList<>();
    private boolean logging_in = false;
    private BroadcastReceiver connection_receiver = new BroadcastReceiver() {
        @Override
        public void onReceive(Context context, Intent intent) {
            Log.i(TAG, "Received connection change event..");
            if (user == null && !logging_in) {
                NetworkInfo activeNetwork = ((ConnectivityManager) getSystemService(Context.CONNECTIVITY_SERVICE)).getActiveNetworkInfo();
                if (activeNetwork != null && activeNetwork.isConnected()) {
                    SharedPreferences shared_prefs = PreferenceManager.getDefaultSharedPreferences(ExPLoRAAService.this);
                    if (shared_prefs.contains(getString(R.string.email)) && shared_prefs.contains(getString(R.string.password)))
                        login(shared_prefs.getString(getString(R.string.email), null), shared_prefs.getString(getString(R.string.password), null));
                }
            }
        }
    };
    private Handler main_handler;

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

        main_handler = new Handler(getApplicationContext().getMainLooper());

        sensor_manager = (SensorManager) getSystemService(Context.SENSOR_SERVICE);
        step_detector = sensor_manager.getDefaultSensor(Sensor.TYPE_STEP_DETECTOR);
        if (step_detector != null) {
            steps_timer = new Timer();
            steps_timer.scheduleAtFixedRate(new TimerTask() {
                @Override
                public void run() {
                    if (mqtt != null && mqtt.isConnected()) {
                        String val = "0";
                        Map<String, String> steps_per_minute = new HashMap<>(1);
                        steps_per_minute.put("steps_per_minute", val);
                        par_vals.get("Steps").put("steps_per_minute", val);
                        try {
                            mqtt.publish(user.id + "/output/Steps", GSON.toJson(steps_per_minute).getBytes(), 1, true);
                        } catch (MqttException e) {
                            Log.w(TAG, "Step-per-minute update MQTT communication failed..", e);
                        }
                        last_step_timestamp = System.currentTimeMillis();
                    }
                }
            }, STEPS_TIME, STEPS_TIME);
        }

        Intent intent = new Intent(this, MainActivity.class);
        PendingIntent pendingIntent = PendingIntent.getActivity(this, 0, intent, 0);
        final Notification notification = new NotificationCompat.Builder(this, getString(R.string.app_name))
                .setSmallIcon(R.drawable.ic_backpacker)
                .setContentTitle(getString(R.string.app_name))
                .setContentIntent(pendingIntent)
                .setGroup(getString(R.string.app_name))
                .setGroupSummary(true)
                .build();
        notification.sound = null;
        startForeground(-1, notification);
    }

    @Override
    public boolean onUnbind(Intent intent) {
        return super.onUnbind(intent);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        Log.i(TAG, "Destroying ExPLoRAA service..");

        if (device_manager != null) {
            device_manager.cleanUp();
        }

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
            if (this.user != null) {
                if (ContextCompat.checkSelfPermission(this, Manifest.permission.ACCESS_FINE_LOCATION) == PackageManager.PERMISSION_GRANTED)
                    ((LocationManager) getSystemService(Context.LOCATION_SERVICE)).removeUpdates(this);
                sensor_manager.unregisterListener(this);

                // gentle disconnection..
                try {
                    if (mqtt.isConnected()) {
                        for (Parameter par : new ArrayList<>(id_par_types.values()))
                            removeParameter(this.user, par);

                        // we clear the following lessons..
                        for (FollowingLessonContext l_ctx : following_lessons) {
                            // we unsubscribe from the lesson's time and state..
                            mqtt.unsubscribe(this.user.id + "/input/lesson-" + l_ctx.getLesson().id + "/time");
                            mqtt.unsubscribe(this.user.id + "/input/lesson-" + l_ctx.getLesson().id + "/state");
                        }

                        // we clear the teachers..
                        for (TeacherContext s_ctx : teachers)
                            mqtt.unsubscribe(s_ctx.getTeacher().id + "/output/on-line");

                        // we clear the teaching lessons..
                        for (TeachingLessonContext l_ctx : teaching_lessons) {
                            // we unsubscribe from the lesson's time and state..
                            mqtt.unsubscribe(this.user.id + "/input/lesson-" + l_ctx.getLesson().id + "/time");
                            mqtt.unsubscribe(this.user.id + "/input/lesson-" + l_ctx.getLesson().id + "/state");
                        }

                        // we clear the students..
                        for (StudentContext s_ctx : students)
                            mqtt.unsubscribe(s_ctx.getStudent().id + "/output/on-line");

                        mqtt.disconnect();
                    }
                    mqtt.close();
                    Log.i(TAG, "Disconnected from the MQTT broker..");
                } catch (MqttException ex) {
                    Log.e(TAG, null, ex);
                } finally {
                    mqtt = null;
                }
            }

            if (user != null) {
                clearAll();
                try {
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
                                    main_handler.post(new Runnable() {
                                        @Override
                                        public void run() {
                                            removeFollowingLesson(id_following_lessons.get(lost_lesson.lesson));
                                        }
                                    });
                                    break;
                                case FollowLesson:
                                    // a new student is following a lesson of this teacher..
                                    final Message.FollowLesson follow_lesson = (Message.FollowLesson) m;
                                    main_handler.post(new Runnable() {
                                        @Override
                                        public void run() {
                                            id_teaching_lessons.get(follow_lesson.lesson).addStudent(new StudentContext(ExPLoRAAService.this, follow_lesson.student));
                                        }
                                    });
                                    break;
                                case UnfollowLesson:
                                    // a student is not following a lesson of this user anymore..
                                    final Message.UnfollowLesson unfollow_lesson = (Message.UnfollowLesson) m;
                                    main_handler.post(new Runnable() {
                                        @Override
                                        public void run() {
                                            id_teaching_lessons.get(unfollow_lesson.lesson).removeStudent(id_students.get(unfollow_lesson.student));
                                        }
                                    });
                                    break;
                                case Token:
                                    // a new token has been created for a teaching lesson..
                                    final Message.Token token = (Message.Token) m;
                                    main_handler.post(new Runnable() {
                                        @Override
                                        public void run() {
                                            id_teaching_lessons.get(token.lesson_id).addToken(token);
                                        }
                                    });
                                    break;
                                case TokenUpdate:
                                    // a token of a teaching lesson has been updated..
                                    final Message.TokenUpdate token_update = (Message.TokenUpdate) m;
                                    main_handler.post(new Runnable() {
                                        @Override
                                        public void run() {
                                            id_teaching_lessons.get(token_update.lesson_id).updateToken(token_update.id, token_update.time, token_update.min, token_update.max);
                                        }
                                    });
                                    break;
                                case RemoveToken:
                                    // a token of a teaching lesson has been removed..
                                    final Message.RemoveToken remove_token = (Message.RemoveToken) m;
                                    main_handler.post(new Runnable() {
                                        @Override
                                        public void run() {
                                            id_teaching_lessons.get(remove_token.lesson_id).removeToken(id_teaching_lessons.get(remove_token.lesson_id).getToken(remove_token.id).getToken());
                                        }
                                    });
                                    break;
                                case Stimulus:
                                    // a new stimulus has been created for a following lesson..
                                    final Message.Stimulus stimulus = (Message.Stimulus) m;
                                    main_handler.post(new Runnable() {
                                        @Override
                                        public void run() {
                                            id_following_lessons.get(stimulus.lesson_id).addStimulus(stimulus);
                                        }
                                    });
                                    break;
                                case RemoveStimulus:
                                    // a stimulus has been removed for a following lesson..
                                    final Message.RemoveStimulus hide_stimulus = (Message.RemoveStimulus) m;
                                    main_handler.post(new Runnable() {
                                        @Override
                                        public void run() {
                                            Message.Stimulus s = null;
                                            for (Message.Stimulus c_s : id_following_lessons.get(hide_stimulus.lesson_id).getStimuli())
                                                if (c_s.id == hide_stimulus.id) {
                                                    s = c_s;
                                                    break;
                                                }
                                            id_following_lessons.get(hide_stimulus.lesson_id).removeStimulus(s);
                                        }
                                    });
                                    break;
                                case Answer:
                                    break;
                                default:
                                    throw new AssertionError(m.message_type.name());
                            }
                        }
                    });

                    for (Parameter par : user.par_types.values()) addParameter(user, par);

                    if (ContextCompat.checkSelfPermission(this, Manifest.permission.ACCESS_FINE_LOCATION) == PackageManager.PERMISSION_GRANTED)
                        ((LocationManager) getSystemService(Context.LOCATION_SERVICE)).requestLocationUpdates(LocationManager.GPS_PROVIDER, 0, 0, this);
                    if (step_detector != null)
                        sensor_manager.registerListener(this, step_detector, SensorManager.SENSOR_DELAY_NORMAL);

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

    private void clearAll() {
        // we clear the current data..
        par_vals.clear();
        id_par_types.clear();

        // we clear the stimuli..
        stimuli.clear();
        for (StimuliListener l : stimuli_listeners) l.stimuliCleared();

        // we clear the following lessons..
        following_lessons.clear();
        id_following_lessons.clear();
        for (FollowingLessonsListener l : following_lessons_listeners)
            l.followingLessonsCleared();

        // we clear the teachers..
        teachers.clear();
        id_teachers.clear();
        for (TeachersListener l : teachers_listeners)
            l.teachersCleared();

        // we clear the models..
        models.clear();

        // we clear the teaching lessons..
        teaching_lessons.clear();
        id_teaching_lessons.clear();
        for (TeachingLessonsListener l : teaching_lessons_listeners)
            l.teachingLessonsCleared();

        // we clear the students..
        students.clear();
        id_students.clear();
        for (StudentsListener l : students_listeners)
            l.studentsCleared();
    }

    private void addParameter(User user, Parameter par) {
        id_par_types.put(par.name, par);
        par_vals.put(par.name, new HashMap<String, String>());
        if (mqtt != null && mqtt.isConnected())
            // we broadcast the existence of a new parameter..
            try {
                mqtt.publish(user.id + "/output", GSON.toJson(new Message.NewParameter(par)).getBytes(), 1, false);
            } catch (MqttException e) {
                Log.w(TAG, "MQTT Connection failed..", e);
            }
    }

    private void removeParameter(User user, Parameter par) {
        id_par_types.remove(par.name);
        if (mqtt != null && mqtt.isConnected())
            // we broadcast the lost of a parameter..
            try {
                mqtt.publish(this.user.id + "/output", GSON.toJson(new Message.RemoveParameter(par.name)).getBytes(), 1, false);
            } catch (MqttException e) {
                Log.w(TAG, "MQTT Connection failed..", e);
            }
    }

    void addStimulus(@NonNull final Message.Stimulus stimulus) {
        final int pos = stimuli.size();
        stimuli.add(stimulus);
        for (StimuliListener listener : stimuli_listeners)
            listener.stimulusAdded(pos, stimulus);

        if (stimuli_listeners.isEmpty()) {
            NotificationManagerCompat notificationManager = NotificationManagerCompat.from(ExPLoRAAService.this);
            TaskStackBuilder task_stack_builder = TaskStackBuilder.create(ExPLoRAAService.this);
            switch (stimulus.stimulus_type) {
                case Text:
                    Intent text_intent = new Intent(ExPLoRAAService.this, TextStimulusActivity.class);
                    text_intent.putExtra("content", ((Message.Stimulus.TextStimulus) stimulus).content);
                    task_stack_builder.addNextIntentWithParentStack(text_intent);
                    PendingIntent text_pending_intent = task_stack_builder.getPendingIntent(pos, PendingIntent.FLAG_UPDATE_CURRENT);

                    //PendingIntent text_pending_intent = PendingIntent.getActivity(ExPLoRAAService.this, stimulus.id, text_intent, PendingIntent.FLAG_UPDATE_CURRENT);
                    final Notification text_notification = new NotificationCompat.Builder(ExPLoRAAService.this, getString(R.string.app_name))
                            .setSmallIcon(R.drawable.ic_backpacker)
                            .setContentTitle(getString(R.string.app_name))
                            .setContentText(((Message.Stimulus.TextStimulus) stimulus).content)
                            .setStyle(new NotificationCompat.BigTextStyle()
                                    .bigText(((Message.Stimulus.TextStimulus) stimulus).content))
                            .setContentIntent(text_pending_intent)
                            .setGroup(getString(R.string.app_name))
                            .setAutoCancel(true)
                            .build();

                    notificationManager.notify(pos, text_notification);
                    break;
                case Question:
                    Intent question_intent = new Intent(ExPLoRAAService.this, QuestionStimulusActivity.class);
                    question_intent.putExtra("question", ((Message.Stimulus.QuestionStimulus) stimulus).question);
                    task_stack_builder.addNextIntentWithParentStack(question_intent);
                    PendingIntent question_pending_intent = task_stack_builder.getPendingIntent(pos, PendingIntent.FLAG_UPDATE_CURRENT);

                    ArrayList<CharSequence> answers = new ArrayList<>(((Message.Stimulus.QuestionStimulus) stimulus).answers.size());
                    answers.addAll(((Message.Stimulus.QuestionStimulus) stimulus).answers);
                    question_intent.putExtra("answers", answers);
                    if (((Message.Stimulus.QuestionStimulus) stimulus).answer != null) {
                        question_intent.putExtra("answer", ((Message.Stimulus.QuestionStimulus) stimulus).answer);
                    }
                    //PendingIntent question_pending_intent = PendingIntent.getActivity(ExPLoRAAService.this, stimulus.id, question_intent, PendingIntent.FLAG_UPDATE_CURRENT);
                    final Notification question_notification = new NotificationCompat.Builder(ExPLoRAAService.this, getString(R.string.app_name))
                            .setSmallIcon(R.drawable.ic_backpacker)
                            .setContentTitle(getString(R.string.app_name))
                            .setContentText(((Message.Stimulus.QuestionStimulus) stimulus).question)
                            .setStyle(new NotificationCompat.BigTextStyle()
                                    .bigText(((Message.Stimulus.QuestionStimulus) stimulus).question))
                            .setContentIntent(question_pending_intent)
                            .setGroup(getString(R.string.app_name))
                            .setAutoCancel(true)
                            .build();

                    notificationManager.notify(pos, question_notification);
                    break;
                case URL:
                    Intent url_intent = new Intent(ExPLoRAAService.this, URLStimulusActivity.class);
                    url_intent.putExtra("content", ((Message.Stimulus.URLStimulus) stimulus).content);
                    url_intent.putExtra("url", ((Message.Stimulus.URLStimulus) stimulus).url);
                    task_stack_builder.addNextIntentWithParentStack(url_intent);
                    PendingIntent url_pending_intent = task_stack_builder.getPendingIntent(pos, PendingIntent.FLAG_UPDATE_CURRENT);

                    //PendingIntent url_pending_intent = PendingIntent.getActivity(ExPLoRAAService.this, stimulus.id, url_intent, PendingIntent.FLAG_UPDATE_CURRENT);
                    final Notification url_notification = new NotificationCompat.Builder(ExPLoRAAService.this, getString(R.string.app_name))
                            .setSmallIcon(R.drawable.ic_backpacker)
                            .setContentTitle(getString(R.string.app_name))
                            .setContentText(((Message.Stimulus.URLStimulus) stimulus).content)
                            .setStyle(new NotificationCompat.BigTextStyle()
                                    .bigText(((Message.Stimulus.URLStimulus) stimulus).content))
                            .setContentIntent(url_pending_intent)
                            .setGroup(getString(R.string.app_name))
                            .setAutoCancel(true)
                            .build();

                    notificationManager.notify(pos, url_notification);
                    break;
            }
        }
    }

    void removeStimulus(@NonNull final Message.Stimulus stimulus) {
        final int pos = stimuli.indexOf(stimulus);
        stimuli.remove(pos);
        for (StimuliListener listener : stimuli_listeners)
            listener.stimulusRemoved(pos, stimulus);

        NotificationManagerCompat.from(ExPLoRAAService.this).cancel(pos);
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
        FollowingLessonContext.FollowingLessonListener following_lesson_listener = new FollowingLessonContext.FollowingLessonListener() {
            @Override
            public void timeChanged(long t) {
                for (FollowingLessonsListener listener : following_lessons_listeners)
                    listener.followingLessonUpdated(pos, l_ctx);
            }

            @Override
            public void stateChanged(Lesson.LessonState state) {
                for (FollowingLessonsListener listener : following_lessons_listeners)
                    listener.followingLessonUpdated(pos, l_ctx);
            }

            @Override
            public void addedStimulus(int position, Message.Stimulus e) {
                for (FollowingLessonsListener listener : following_lessons_listeners)
                    listener.followingLessonUpdated(pos, l_ctx);
            }

            @Override
            public void removedStimulus(int position, Message.Stimulus e) {
                for (FollowingLessonsListener listener : following_lessons_listeners)
                    listener.followingLessonUpdated(pos, l_ctx);
            }
        };
        l_ctx.addListener(following_lesson_listener);
        id_following_lesson_listener.put(l_ctx.getLesson().id, following_lesson_listener);
        try {
            // we subscribe to the lesson's time..
            mqtt.subscribe(l_ctx.getLesson().teacher.user.id + "/input/lesson-" + l_ctx.getLesson().id + "/time", new IMqttMessageListener() {
                @Override
                public void messageArrived(final String topic, final MqttMessage message) {
                    main_handler.post(new Runnable() {
                        @Override
                        public void run() {
                            l_ctx.setTime(Long.parseLong(new String(message.getPayload())));
                        }
                    });
                }
            });
            // we subscribe to the lesson's state..
            mqtt.subscribe(l_ctx.getLesson().teacher.user.id + "/input/lesson-" + l_ctx.getLesson().id + "/state", new IMqttMessageListener() {
                @Override
                public void messageArrived(final String topic, final MqttMessage message) {
                    main_handler.post(new Runnable() {
                        @Override
                        public void run() {
                            l_ctx.setState(Lesson.LessonState.valueOf(new String(message.getPayload())));
                        }
                    });
                }
            });
        } catch (MqttException ex) {
            Log.w(TAG, "MQTT subscription failed..", ex);
        }
        for (FollowingLessonsListener listener : following_lessons_listeners)
            listener.followingLessonAdded(pos, l_ctx);
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
        l_ctx.removeListener(id_following_lesson_listener.get(l_ctx.getLesson().id));
        id_following_lesson_listener.remove(l_ctx.getLesson().id);
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
        for (FollowingLessonsListener listener : following_lessons_listeners)
            listener.followingLessonRemoved(pos, l_ctx);
    }

    private void addTeacher(@NonNull final TeacherContext t_ctx) {
        final int pos = teachers.size();
        teachers.add(t_ctx);
        id_teachers.put(t_ctx.getTeacher().id, t_ctx);
        TeacherContext.TeacherListener l = new TeacherContext.TeacherListener() {
            @Override
            public void onlineChanged(boolean on_line) {
                for (TeachersListener listener : teachers_listeners)
                    listener.teacherUpdated(pos, t_ctx);
            }
        };
        t_ctx.addListener(l);
        id_teacher_listener.put(t_ctx.getTeacher().id, l);
        try {
            mqtt.subscribe(t_ctx.getTeacher().id + "/output/on-line", new IMqttMessageListener() {
                @Override
                public void messageArrived(final String topic, final MqttMessage message) {
                    main_handler.post(new Runnable() {
                        @Override
                        public void run() {
                            t_ctx.setOnLine(Boolean.parseBoolean(new String(message.getPayload())));
                        }
                    });
                }
            });
        } catch (MqttException ex) {
            Log.w(TAG, "MQTT subscription failed..", ex);
        }
        for (TeachersListener listener : teachers_listeners)
            listener.teacherAdded(pos, t_ctx);
    }

    private void removeTeacher(@NonNull final TeacherContext t_ctx) {
        int pos = teachers.indexOf(t_ctx);
        teachers.remove(pos);
        id_teachers.remove(t_ctx.getTeacher().id);
        t_ctx.removeListener(id_teacher_listener.get(t_ctx.getTeacher().id));
        id_teacher_listener.remove(t_ctx.getTeacher().id);
        if (mqtt.isConnected()) try {
            // we might be removing teachers as a consequence of a connection loss..
            mqtt.unsubscribe(t_ctx.getTeacher().id + "/output/on-line");
        } catch (MqttException ex) {
            Log.w(TAG, "MQTT unsubscription failed..", ex);
        }
        for (TeachersListener listener : teachers_listeners)
            listener.teacherRemoved(pos, t_ctx);
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
        TeachingLessonContext.TeachingLessonListener l = new TeachingLessonContext.TeachingLessonListener() {
            @Override
            public void timeChanged(long t) {
                for (TeachingLessonsListener listener : teaching_lessons_listeners)
                    listener.teachingLessonUpdated(pos, l_ctx);
            }

            @Override
            public void stateChanged(Lesson.LessonState state) {
                for (TeachingLessonsListener listener : teaching_lessons_listeners)
                    listener.teachingLessonUpdated(pos, l_ctx);
            }

            @Override
            public void studentAdded(int pos, StudentContext s_ctx) {
                for (TeachingLessonsListener listener : teaching_lessons_listeners)
                    listener.teachingLessonUpdated(pos, l_ctx);
            }

            @Override
            public void studentRemoved(int pos, StudentContext s_ctx) {
                for (TeachingLessonsListener listener : teaching_lessons_listeners)
                    listener.teachingLessonUpdated(pos, l_ctx);
            }

            @Override
            public void addedToken(int pos, TeachingLessonContext.TokenRow tk) {
                for (TeachingLessonsListener listener : teaching_lessons_listeners)
                    listener.teachingLessonUpdated(pos, l_ctx);
            }

            @Override
            public void removedToken(int pos, TeachingLessonContext.TokenRow tk) {
                for (TeachingLessonsListener listener : teaching_lessons_listeners)
                    listener.teachingLessonUpdated(pos, l_ctx);
            }

            @Override
            public void updatedToken(int pos, TeachingLessonContext.TokenRow tk) {
                for (TeachingLessonsListener listener : teaching_lessons_listeners)
                    listener.teachingLessonUpdated(pos, l_ctx);
            }
        };
        l_ctx.addListener(l);
        id_teaching_lesson_listener.put(l_ctx.getLesson().id, l);
        try {
            // we subscribe to the lesson's time..
            mqtt.subscribe(user.id + "/input/lesson-" + l_ctx.getLesson().id + "/time", new IMqttMessageListener() {
                @Override
                public void messageArrived(final String topic, final MqttMessage message) {
                    main_handler.post(new Runnable() {
                        @Override
                        public void run() {
                            l_ctx.setTime(Long.parseLong(new String(message.getPayload())));
                        }
                    });
                }
            });
            // we subscribe to the lesson's state..
            mqtt.subscribe(user.id + "/input/lesson-" + l_ctx.getLesson().id + "/state", new IMqttMessageListener() {
                @Override
                public void messageArrived(final String topic, final MqttMessage message) {
                    main_handler.post(new Runnable() {
                        @Override
                        public void run() {
                            l_ctx.setState(Lesson.LessonState.valueOf(new String(message.getPayload())));
                        }
                    });
                }
            });
        } catch (MqttException ex) {
            Log.w(TAG, "MQTT subscription failed..", ex);
        }
        for (TeachingLessonsListener listener : teaching_lessons_listeners)
            listener.teachingLessonAdded(pos, l_ctx);
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
        l_ctx.removeListener(id_teaching_lesson_listener.get(l_ctx.getLesson().id));
        id_teaching_lesson_listener.remove(l_ctx.getLesson().id);
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
        for (TeachingLessonsListener listener : teaching_lessons_listeners)
            listener.teachingLessonRemoved(pos, l_ctx);
    }

    void addStudent(@NonNull final StudentContext s_ctx) {
        final int pos = teachers.size();
        students.add(s_ctx);
        id_students.put(s_ctx.getStudent().id, s_ctx);
        StudentContext.StudentListener l = new StudentContext.StudentListener() {
            @Override
            public void onlineChanged(boolean on_line) {
                for (StudentsListener listener : students_listeners)
                    listener.studentUpdated(pos, s_ctx);
            }
        };
        s_ctx.addListener(l);
        id_student_listener.put(s_ctx.getStudent().id, l);
        try {
            // we subscribe to be notified whether the student gets online/offline..
            mqtt.subscribe(s_ctx.getStudent().id + "/output/on-line", new IMqttMessageListener() {
                @Override
                public void messageArrived(final String topic, final MqttMessage message) {
                    main_handler.post(new Runnable() {
                        @Override
                        public void run() {
                            s_ctx.setOnLine(Boolean.parseBoolean(new String(message.getPayload())));
                        }
                    });
                }
            });

            // TODO: register to the student's parameters..
        } catch (MqttException ex) {
            Log.w(TAG, "MQTT subscription failed..", ex);
        }
        for (StudentsListener listener : students_listeners)
            listener.studentAdded(pos, s_ctx);
    }

    void removeStudent(@NonNull final StudentContext s_ctx) {
        int pos = students.indexOf(s_ctx);
        students.remove(pos);
        id_students.remove(s_ctx.getStudent().id);
        s_ctx.removeListener(id_student_listener.get(s_ctx.getStudent().id));
        id_student_listener.remove(s_ctx.getStudent().id);
        try {
            if (mqtt.isConnected()) {
                mqtt.unsubscribe(s_ctx.getStudent().id + "/output/on-line");
            }
        } catch (MqttException ex) {
            Log.w(TAG, "MQTT unsubscription failed..", ex);
        }
        for (StudentsListener listener : students_listeners)
            listener.studentRemoved(pos, s_ctx);
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
                Intent user_creation_intent = new Intent(USER_CREATION);
                user_creation_intent.putExtra("successful", false);
                sendBroadcast(user_creation_intent);
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
                Intent login_intent = new Intent(LOGIN);
                login_intent.putExtra("successful", false);
                sendBroadcast(login_intent);
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
        disconnectEmpaticaE4();
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

    public void answerQuestion(final long lesson_id, final int question_id, final int answer) {
        resource.answer_question(user.id, lesson_id, question_id, answer).enqueue(new Callback<Void>() {
            @Override
            public void onResponse(Call<Void> call, Response<Void> response) {
                if (!response.isSuccessful()) try {
                    Toast.makeText(ExPLoRAAService.this, response.errorBody().string(), Toast.LENGTH_SHORT).show();
                } catch (IOException e) {
                    e.printStackTrace();
                }
                else {
                    FollowingLessonContext l_ctx = id_following_lessons.get(lesson_id);
                    Message.Stimulus.QuestionStimulus q = null;
                    for (Message.Stimulus st : stimuli) {
                        if (st.id == question_id) {
                            q = (Message.Stimulus.QuestionStimulus) st;
                            break;
                        }
                    }
                    q.answer = answer;
                }
            }

            @Override
            public void onFailure(Call<Void> call, Throwable t) {
                Log.e(TAG, null, t);
            }
        });
    }

    public void connectEmpaticaE4() {
        device_manager = new EmpaDeviceManager(getApplicationContext(), this, this);
        device_manager.authenticateWithAPIKey(BuildConfig.EMPATICA_API_KEY);
    }

    public void disconnectEmpaticaE4() {
        if (device_manager != null) {
            device_manager.disconnect();
            device_manager = null;
        }
    }

    @Override
    public void onLocationChanged(Location location) {
        if (mqtt != null && mqtt.isConnected() && isBetterLocation(location)) {
            String latitude = Double.toString(location.getLatitude());
            String longitude = Double.toString(location.getLatitude());
            Map<String, String> gps_pos = new HashMap<>(2);
            gps_pos.put("latitude", latitude);
            gps_pos.put("longitude", longitude);
            par_vals.get("GPS").put("latitude", latitude);
            par_vals.get("GPS").put("longitude", longitude);
            try {
                mqtt.publish(user.id + "/output/GPS", GSON.toJson(gps_pos).getBytes(), 1, true);
            } catch (MqttException e) {
                Log.w(TAG, "GPS update MQTT communication failed..", e);
            }
            current_location = location;
        }
    }

    /**
     * Determines whether one Location reading is better than the current Location fix
     *
     * @param location The new Location that you want to evaluate
     */
    private boolean isBetterLocation(Location location) {
        if (current_location == null) {
            // A new location is always better than no location
            return true;
        } else if (current_location.distanceTo(location) < 5) {
            // less than 5 meters from last known location..
            return false;
        }

        // Check whether the new location fix is newer or older
        long time_delta = location.getTime() - current_location.getTime();
        boolean is_significantly_newer = time_delta > LOCATION_TIME;
        boolean is_significantly_older = time_delta < -LOCATION_TIME;
        boolean is_newer = time_delta > 0;

        // If it's been more than two minutes since the current location, use the new location
        // because the user has likely moved
        if (is_significantly_newer) {
            return true;
            // If the new location is more than two minutes older, it must be worse
        } else if (is_significantly_older) {
            return false;
        }

        // Check whether the new location fix is more or less accurate
        int accuracy_delta = (int) (location.getAccuracy() - current_location.getAccuracy());
        boolean is_less_accurate = accuracy_delta > 0;
        boolean is_more_accurate = accuracy_delta < 0;
        boolean is_significantly_less_accurate = accuracy_delta > 200;

        // Check if the old and new location are from the same provider
        boolean is_from_same_provider = isSameProvider(location.getProvider(), current_location.getProvider());

        // Determine location quality using a combination of timeliness and accuracy
        if (is_more_accurate) {
            return true;
        } else if (is_newer && !is_less_accurate) {
            return true;
        } else if (is_newer && !is_significantly_less_accurate && is_from_same_provider) {
            return true;
        }
        return false;
    }

    /**
     * Checks whether two providers are the same
     */
    private boolean isSameProvider(String provider1, String provider2) {
        if (provider1 == null) {
            return provider2 == null;
        }
        return provider1.equals(provider2);
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

    @Override
    public void onSensorChanged(SensorEvent sensorEvent) {
        switch (sensorEvent.sensor.getType()) {
            case Sensor.TYPE_STEP_DETECTOR:
                Log.i(TAG, "New step..");
                steps_timer.cancel();
                steps_timer = new Timer();
                steps_timer.scheduleAtFixedRate(new TimerTask() {
                    @Override
                    public void run() {
                        if (mqtt != null && mqtt.isConnected()) {
                            String val = "0";
                            Map<String, String> steps_per_minute = new HashMap<>(1);
                            steps_per_minute.put("steps_per_minute", val);
                            par_vals.get("Steps").put("steps_per_minute", val);
                            try {
                                mqtt.publish(user.id + "/output/Steps", GSON.toJson(steps_per_minute).getBytes(), 1, true);
                            } catch (MqttException e) {
                                Log.w(TAG, "Step-per-minute update MQTT communication failed..", e);
                            }
                            last_step_timestamp = System.currentTimeMillis();
                        }
                    }
                }, STEPS_TIME, STEPS_TIME);
                if (mqtt != null && mqtt.isConnected()) {
                    String val = Double.toString(60000D / (sensorEvent.timestamp - last_step_timestamp));
                    Map<String, String> steps_per_minute = new HashMap<>(1);
                    steps_per_minute.put("steps_per_minute", val);
                    par_vals.get("Steps").put("steps_per_minute", val);
                    try {
                        mqtt.publish(user.id + "/output/Steps", GSON.toJson(steps_per_minute).getBytes(), 1, true);
                    } catch (MqttException e) {
                        Log.w(TAG, "Step-per-minute update MQTT communication failed..", e);
                    }
                    last_step_timestamp = sensorEvent.timestamp;
                }
        }
    }

    @Override
    public void onAccuracyChanged(Sensor sensor, int i) {
    }

    @Override
    public void didReceiveGSR(float gsr, double timestamp) {
        Log.i(TAG, "Galvanic Skin Response (GSR): " + gsr);
    }

    @Override
    public void didReceiveBVP(float bvp, double timestamp) {
        Log.i(TAG, "Blood Volume Pulse (BVP): " + bvp);
    }

    @Override
    public void didReceiveIBI(float ibi, double timestamp) {
        Log.i(TAG, "Interbeat interval (IBI): " + ibi);
    }

    @Override
    public void didReceiveTemperature(float t, double timestamp) {
        Log.i(TAG, "Temperature: " + t);
    }

    @Override
    public void didReceiveAcceleration(int x, int y, int z, double timestamp) {
        Log.i(TAG, "Acceleration: [" + x + ", " + y + ", " + z + "]");
    }

    @Override
    public void didReceiveBatteryLevel(float level, double timestamp) {
        Log.i(TAG, "Battery level: " + level);
    }

    @Override
    public void didReceiveTag(double timestamp) {
        Log.i(TAG, "Tag..");
    }

    @Override
    public void didUpdateStatus(EmpaStatus status) {
        switch (status) {
            case READY:
                Log.i(TAG, "Empatica E4 manager ready..");
                device_manager.startScanning();
                break;
            case DISCONNECTED:
                Log.i(TAG, "Empatica E4 disconnected device..");
                main_handler.post(new Runnable() {
                    @Override
                    public void run() {
                        Toast.makeText(ExPLoRAAService.this, "Empatica E4 device is disconnected!", Toast.LENGTH_LONG).show();
                        for (EmpaticaE4Listener l : empatica_e4_listeners)
                            l.empaticaE4Disconnected();
                    }
                });
                break;
            case CONNECTING:
                Log.i(TAG, "Empatica E4 connecting device..");
                // we add the Galvanic Skin Response (GSR) sensor..
                Parameter empatica_e4_gsr = new Parameter();
                empatica_e4_gsr.name = "EmpaticaE4_GSR";
                empatica_e4_gsr.properties = new HashMap<>(1);
                empatica_e4_gsr.properties.put("GSR", "numeric");
                addParameter(user, empatica_e4_gsr);

                // we add the Blood Volume Pulse (BVP) sensor..
                Parameter empatica_e4_bvp = new Parameter();
                empatica_e4_bvp.name = "EmpaticaE4_BVP";
                empatica_e4_bvp.properties = new HashMap<>(1);
                empatica_e4_bvp.properties.put("BVP", "numeric");
                addParameter(user, empatica_e4_bvp);

                // we add the Interbeat interval (IBI) sensor..
                Parameter empatica_e4_ibi = new Parameter();
                empatica_e4_ibi.name = "EmpaticaE4_IBI";
                empatica_e4_ibi.properties = new HashMap<>(1);
                empatica_e4_ibi.properties.put("IBI", "numeric");
                addParameter(user, empatica_e4_ibi);

                // we add the Temperature sensor..
                Parameter empatica_e4_temperature = new Parameter();
                empatica_e4_temperature.name = "EmpaticaE4_Temperature";
                empatica_e4_temperature.properties = new HashMap<>(1);
                empatica_e4_temperature.properties.put("Temperature", "numeric");
                addParameter(user, empatica_e4_temperature);

                // we add the Acceleration sensor..
                Parameter empatica_e4_acceleration = new Parameter();
                empatica_e4_acceleration.name = "EmpaticaE4_Acceleration";
                empatica_e4_acceleration.properties = new HashMap<>(3);
                empatica_e4_acceleration.properties.put("x", "numeric");
                empatica_e4_acceleration.properties.put("y", "numeric");
                empatica_e4_acceleration.properties.put("z", "numeric");
                addParameter(user, empatica_e4_acceleration);

                // we add the Battery sensor..
                Parameter empatica_e4_battery = new Parameter();
                empatica_e4_battery.name = "EmpaticaE4_Battery";
                empatica_e4_battery.properties = new HashMap<>(1);
                empatica_e4_battery.properties.put("Level", "numeric");
                addParameter(user, empatica_e4_battery);
                break;
            case CONNECTED:
                Log.i(TAG, "Empatica E4 connected device..");
                main_handler.post(new Runnable() {
                    @Override
                    public void run() {
                        Toast.makeText(ExPLoRAAService.this, "Empatica E4 device is connected!", Toast.LENGTH_LONG).show();
                        for (EmpaticaE4Listener l : empatica_e4_listeners) l.empaticaE4Connected();
                    }
                });
                break;
            case DISCONNECTING:
                Log.i(TAG, "Empatica E4 disconnecting device..");
                removeParameter(user, id_par_types.get("EmpaticaE4_Acceleration"));
                removeParameter(user, id_par_types.get("EmpaticaE4_Temperature"));
                removeParameter(user, id_par_types.get("EmpaticaE4_IBI"));
                removeParameter(user, id_par_types.get("EmpaticaE4_BVP"));
                removeParameter(user, id_par_types.get("EmpaticaE4_GSR"));
                removeParameter(user, id_par_types.get("EmpaticaE4_Battery"));
                break;
            case DISCOVERING:
                Log.i(TAG, "Empatica E4 manager discovering devices..");
                main_handler.post(new Runnable() {
                    @Override
                    public void run() {
                        Toast.makeText(ExPLoRAAService.this, "Searching of an Empatica E4 device..", Toast.LENGTH_LONG).show();
                    }
                });
                break;
        }
    }

    @Override
    public void didEstablishConnection() {
    }

    @Override
    public void didUpdateSensorStatus(int status, EmpaSensorType type) {
    }

    @Override
    public void didDiscoverDevice(EmpaticaDevice device, String deviceLabel, int rssi, boolean allowed) {
        Log.i(TAG, "Discovered device " + deviceLabel);
        if (allowed) {
            device_manager.stopScanning();
            try {
                device_manager.connectDevice(device);
            } catch (ConnectionNotAllowedException e) {
                Log.w(TAG, e);
            }
        }
    }

    @Override
    public void didRequestEnableBluetooth() {
    }

    @Override
    public void didUpdateOnWristStatus(int status) {
    }

    private Map<String, Parameter> get_par_types() {
        Map<String, Parameter> c_par_types = new HashMap<>();

        // the GPS sensor..
        if (ContextCompat.checkSelfPermission(this, Manifest.permission.ACCESS_FINE_LOCATION) == PackageManager.PERMISSION_GRANTED) {
            Parameter gps = new Parameter();
            gps.name = "GPS";
            gps.properties = new HashMap<>(2);
            gps.properties.put("latitude", "numeric");
            gps.properties.put("longitude", "numeric");
            c_par_types.put("GPS", gps);
        }

        // the step detector sensor..
        if (step_detector != null) {
            Parameter steps = new Parameter();
            steps.name = "Steps";
            steps.properties = new HashMap<>(1);
            steps.properties.put("steps_per_minute", "numeric");
            c_par_types.put("Steps", steps);
        }
        return c_par_types;
    }

    private Map<String, Map<String, String>> get_par_values() {
        Map<String, Map<String, String>> c_par_values = new HashMap<>();

        // the GPS sensor..
        if (ContextCompat.checkSelfPermission(this, Manifest.permission.ACCESS_FINE_LOCATION) == PackageManager.PERMISSION_GRANTED) {
            Map<String, String> gps_pos = new HashMap<>(2);
            final Location last_location = ((LocationManager) getSystemService(Context.LOCATION_SERVICE)).getLastKnownLocation(LocationManager.GPS_PROVIDER);
            if (last_location != null && isBetterLocation(last_location)) {
                gps_pos.put("latitude", Double.toString(last_location.getLatitude()));
                gps_pos.put("longitude", Double.toString(last_location.getLongitude()));
                current_location = last_location;
            } else {
                gps_pos.put("latitude", Double.toString(0));
                gps_pos.put("longitude", Double.toString(0));
            }
            c_par_values.put("GPS", gps_pos);
        }

        // the step detector sensor..
        if (step_detector != null) {
            Map<String, String> steps_per_minute = new HashMap<>(1);
            steps_per_minute.put("steps_per_minute", "0");
            c_par_values.put("Steps", steps_per_minute);
        }

        return c_par_values;
    }

    public void addEmpaticaE4Listener(EmpaticaE4Listener l) {
        empatica_e4_listeners.add(l);
    }

    public void removeEmpaticaE4Listener(EmpaticaE4Listener l) {
        empatica_e4_listeners.remove(l);
    }

    public void addStimuliListener(StimuliListener l) {
        stimuli_listeners.add(l);
    }

    public void removeStimuliListener(StimuliListener l) {
        stimuli_listeners.remove(l);
    }

    public void addFollowingLessonsListener(FollowingLessonsListener l) {
        following_lessons_listeners.add(l);
    }

    public void removeFollowingLessonsListener(FollowingLessonsListener l) {
        following_lessons_listeners.remove(l);
    }

    public void addTeachersListener(TeachersListener l) {
        teachers_listeners.add(l);
    }

    public void removeTeachersListener(TeachersListener l) {
        teachers_listeners.remove(l);
    }

    public void addTeachingLessonsListener(TeachingLessonsListener l) {
        teaching_lessons_listeners.add(l);
    }

    public void removeTeachingLessonsListener(TeachingLessonsListener l) {
        teaching_lessons_listeners.remove(l);
    }

    public void addStudentsListener(StudentsListener l) {
        students_listeners.add(l);
    }

    public void removeStudentsListener(StudentsListener l) {
        students_listeners.remove(l);
    }

    public class ExPLoRAABinder extends Binder {

        public ExPLoRAAService getService() {
            return ExPLoRAAService.this;
        }
    }

    public interface EmpaticaE4Listener {

        void empaticaE4Connected();

        void empaticaE4Disconnected();
    }

    public interface StimuliListener {

        void stimulusAdded(int pos, Message.Stimulus stimulus);

        void stimulusRemoved(int pos, Message.Stimulus stimulus);

        void stimuliCleared();
    }

    public interface FollowingLessonsListener {

        void followingLessonAdded(int pos, FollowingLessonContext ctx);

        void followingLessonUpdated(int pos, FollowingLessonContext ctx);

        void followingLessonRemoved(int pos, FollowingLessonContext ctx);

        void followingLessonsCleared();
    }

    public interface TeachersListener {

        void teacherAdded(int pos, TeacherContext ctx);

        void teacherUpdated(int pos, TeacherContext ctx);

        void teacherRemoved(int pos, TeacherContext ctx);

        void teachersCleared();
    }

    public interface TeachingLessonsListener {

        void teachingLessonAdded(int pos, TeachingLessonContext ctx);

        void teachingLessonUpdated(int pos, TeachingLessonContext ctx);

        void teachingLessonRemoved(int pos, TeachingLessonContext ctx);

        void teachingLessonsCleared();
    }

    public interface StudentsListener {

        void studentAdded(int pos, StudentContext ctx);

        void studentUpdated(int pos, StudentContext ctx);

        void studentRemoved(int pos, StudentContext ctx);

        void studentsCleared();
    }
}
