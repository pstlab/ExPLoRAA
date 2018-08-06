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
import android.os.Handler;
import android.os.Looper;
import android.support.annotation.NonNull;
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
import java.util.concurrent.ExecutionException;

import it.cnr.istc.exploraa.api.ExPLoRAA;
import it.cnr.istc.exploraa.api.Follow;
import it.cnr.istc.exploraa.api.Lesson;
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
    public static final Gson GSON = new GsonBuilder().registerTypeAdapter(Message.class, Message.ADAPTER).registerTypeAdapter(LessonModel.class, LessonModel.ADAPTER).create();
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
    private final Collection<StimuliListener> stimuli_listeners = new ArrayList<>();
    private final Collection<FollowingLessonsListener> following_lessons_listeners = new ArrayList<>();
    private final Collection<TeachersListener> teachers_listeners = new ArrayList<>();
    private final Collection<TeachingLessonsListener> teaching_lessons_listeners = new ArrayList<>();
    private final Collection<StudentsListener> students_listeners = new ArrayList<>();
    private MqttClient mqtt;
    /**
     * The current user.
     */
    private User user;

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
                        mqtt.publish(this.user.id + "/output", GSON.toJson(new Message.RemoveParameter(par.name)).getBytes(), 1, false);
                    new Handler(Looper.getMainLooper()).post(new Runnable() {
                        @Override
                        public void run() {
                            if (ContextCompat.checkSelfPermission(ctx, Manifest.permission.ACCESS_FINE_LOCATION) == PackageManager.PERMISSION_GRANTED)
                                ((LocationManager) ctx.getSystemService(Context.LOCATION_SERVICE)).removeUpdates(ExPLoRAAContext.this);
                        }
                    });
                    par_types.clear();
                    id_par_types.clear();
                    stimuli.clear();
                    for (StimuliListener l : stimuli_listeners) l.stimuliCleared();
                    if (mqtt.isConnected()) for (FollowingLessonContext l_ctx : following_lessons) {
                        // we unsubscribe from the lesson's time and state..
                        mqtt.unsubscribe(this.user.id + "/input/lesson-" + l_ctx.getLesson().id + "/time");
                        mqtt.unsubscribe(this.user.id + "/input/lesson-" + l_ctx.getLesson().id + "/state");
                    }

                    for (FollowingLessonContext l_ctx : following_lessons)
                        l_ctx.removeListener(id_following_lesson_listener.get(l_ctx.getLesson().id));
                    following_lessons.clear();
                    id_following_lessons.clear();
                    id_following_lesson_listener.clear();
                    for (FollowingLessonsListener l : following_lessons_listeners)
                        l.followingLessonsCleared();

                    for (TeacherContext t_ctx : teachers)
                        t_ctx.removeListener(id_teacher_listener.get(t_ctx.getTeacher().id));
                    teachers.clear();
                    id_teachers.clear();
                    id_teacher_listener.clear();
                    for (TeachersListener l : teachers_listeners) l.teachersCleared();

                    models.clear();
                    // we unsubscribe from the lesson's time and state..
                    for (TeachingLessonContext l_ctx : teaching_lessons) {
                        if (mqtt.isConnected()) {
                            mqtt.unsubscribe(this.user.id + "/input/lesson-" + l_ctx.getLesson().id + "/time");
                            mqtt.unsubscribe(this.user.id + "/input/lesson-" + l_ctx.getLesson().id + "/state");
                        }
                        l_ctx.removeListener(id_teaching_lesson_listener.get(l_ctx.getLesson().id));
                    }
                    teaching_lessons.clear();
                    id_teaching_lessons.clear();
                    id_teaching_lesson_listener.clear();
                    for (TeachingLessonsListener l : teaching_lessons_listeners)
                        l.teachingLessonsCleared();

                    for (StudentContext s_ctx : students)
                        s_ctx.removeListener(id_student_listener.get(s_ctx.getStudent().id));
                    students.clear();
                    id_students.clear();
                    id_student_listener.clear();
                    for (StudentsListener l : students_listeners) l.studentsCleared();

                    if (mqtt.isConnected()) mqtt.disconnect();
                    mqtt.close();
                } catch (MqttException ex) {
                    Log.e(TAG, null, ex);
                }
            }

            if (user != null) try {
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
                        Log.d(TAG, "Message arrived: " + topic + " - " + message);
                        Message m = GSON.fromJson(new String(message.getPayload()), Message.class);
                        switch (m.message_type) {
                            case RemoveLesson:
                                // a teacher has removed a lesson for this student..
                                final Message.RemoveLesson lost_lesson = (Message.RemoveLesson) m;
                                new Handler(Looper.getMainLooper()).post(new Runnable() {
                                    @Override
                                    public void run() {
                                        removeFollowingLesson(id_following_lessons.get(lost_lesson.lesson));
                                    }
                                });
                                break;
                            case FollowLesson:
                                // a new student is following a lesson of this teacher..
                                final Message.FollowLesson follow_lesson = (Message.FollowLesson) m;
                                new Handler(Looper.getMainLooper()).post(new Runnable() {
                                    @Override
                                    public void run() {
                                        id_teaching_lessons.get(follow_lesson.lesson).addStudent(new StudentContext(follow_lesson.student));
                                    }
                                });
                                break;
                            case UnfollowLesson:
                                // a student is not following a lesson of this user anymore..
                                final Message.UnfollowLesson unfollow_lesson = (Message.UnfollowLesson) m;
                                new Handler(Looper.getMainLooper()).post(new Runnable() {
                                    @Override
                                    public void run() {
                                        id_teaching_lessons.get(unfollow_lesson.lesson).removeStudent(id_students.get(unfollow_lesson.student));
                                        Set<Long> c_students = new HashSet<>();
                                        for (TeachingLessonContext l : teaching_lessons)
                                            for (StudentContext student : l.getStudents())
                                                c_students.add(student.getStudent().id);
                                        if (!c_students.contains(unfollow_lesson.student))
                                            removeStudent(id_students.get(unfollow_lesson.student));
                                    }
                                });
                                break;
                            case Token:
                                // a new token has been created for a teaching lesson..
                                final Message.Token token = (Message.Token) m;
                                new Handler(Looper.getMainLooper()).post(new Runnable() {
                                    @Override
                                    public void run() {
                                        id_teaching_lessons.get(token.lesson_id).addToken(token);
                                    }
                                });
                                break;
                            case TokenUpdate:
                                // a token of a teaching lesson has been updated..
                                final Message.TokenUpdate token_update = (Message.TokenUpdate) m;
                                new Handler(Looper.getMainLooper()).post(new Runnable() {
                                    @Override
                                    public void run() {
                                        id_teaching_lessons.get(token_update.lesson_id).updateToken(token_update.id, token_update.time, token_update.min != null ? token_update.min : Long.MIN_VALUE, token_update.max != null ? token_update.max : Long.MAX_VALUE);
                                    }
                                });
                                break;
                            case RemoveToken:
                                // a token of a teaching lesson has been removed..
                                final Message.RemoveToken remove_token = (Message.RemoveToken) m;
                                new Handler(Looper.getMainLooper()).post(new Runnable() {
                                    @Override
                                    public void run() {
                                        id_teaching_lessons.get(remove_token.lesson_id).removeToken(id_teaching_lessons.get(remove_token.lesson_id).getToken(remove_token.id).getToken());
                                    }
                                });
                                break;
                            case Stimulus:
                                // a new stimulus has been created for a following lesson..
                                final Message.Stimulus stimulus = (Message.Stimulus) m;
                                new Handler(Looper.getMainLooper()).post(new Runnable() {
                                    @Override
                                    public void run() {
                                        id_following_lessons.get(stimulus.lesson_id).addStimulus(stimulus);
                                    }
                                });
                                break;
                            case RemoveStimulus:
                                // a stimulus has been removed for a following lesson..
                                final Message.RemoveStimulus hide_stimulus = (Message.RemoveStimulus) m;
                                new Handler(Looper.getMainLooper()).post(new Runnable() {
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

                for (Parameter par : user.par_types.values()) {
                    par_types.add(par);
                    // we broadcast the existence of a new parameter..
                    mqtt.publish(user.id + "/output", GSON.toJson(new Message.NewParameter(par)).getBytes(), 1, false);
                }
                new Handler(Looper.getMainLooper()).post(new Runnable() {
                    @Override
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
            this.user = user;
        }
    }

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

    public List<Message.Stimulus> getStimuli() {
        return Collections.unmodifiableList(stimuli);
    }

    void addStimulus(@NonNull final Message.Stimulus stimulus) {
        final int pos = stimuli.size();
        stimuli.add(stimulus);
        for (StimuliListener listener : stimuli_listeners)
            listener.stimulusAdded(pos, stimulus);
    }

    private void addFollowingLesson(@NonNull final FollowingLessonContext l_ctx) {
        final int pos = following_lessons.size();
        following_lessons.add(l_ctx);
        id_following_lessons.put(l_ctx.getLesson().id, l_ctx);
        if (id_teachers.get(l_ctx.getLesson().teacher.user.id) == null) {
            final TeacherContext t_ctx = new TeacherContext(l_ctx.getLesson().teacher.user);
            teachers.add(t_ctx);
            id_teachers.put(t_ctx.getTeacher().id, t_ctx);
        }
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
                    new Handler(Looper.getMainLooper()).post(new Runnable() {
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
                    new Handler(Looper.getMainLooper()).post(new Runnable() {
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

    void removeStimulus(@NonNull final Message.Stimulus stimulus) {
        final int pos = stimuli.indexOf(stimulus);
        stimuli.remove(pos);
        for (StimuliListener listener : stimuli_listeners)
            listener.stimulusRemoved(pos, stimulus);
    }

    private void removeFollowingLesson(@NonNull final FollowingLessonContext l_ctx) {
        for (Message.Stimulus st : l_ctx.getStimuli()) removeStimulus(st);
        int pos = following_lessons.indexOf(l_ctx);
        following_lessons.remove(pos);
        id_following_lessons.remove(l_ctx.getLesson().id);
        l_ctx.removeListener(id_following_lesson_listener.get(l_ctx.getLesson().id));
        id_following_lesson_listener.remove(l_ctx.getLesson().id);
        if (mqtt.isConnected()) {
            try {
                // we subscribe from the lesson's time and state..
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

    public FollowingLessonContext getFollowingLesson(final long id) {
        return id_following_lessons.get(id);
    }

    public List<FollowingLessonContext> getFollowingLessons() {
        return Collections.unmodifiableList(following_lessons);
    }

    public List<LessonModel> getModels() {
        return Collections.unmodifiableList(models);
    }

    private void addTeachingLesson(@NonNull final TeachingLessonContext l_ctx) {
        final int l_pos = teaching_lessons.size();
        teaching_lessons.add(l_ctx);
        id_teaching_lessons.put(l_ctx.getLesson().id, l_ctx);
        if (l_ctx.getLesson().students != null)
            for (Follow follow : l_ctx.getLesson().students.values())
                l_ctx.addStudent(id_students.get(follow.user.id) != null ? id_students.get(follow.user.id) : new StudentContext(follow.user));
        TeachingLessonContext.TeachingLessonListener l = new TeachingLessonContext.TeachingLessonListener() {
            @Override
            public void timeChanged(long t) {
                for (TeachingLessonsListener listener : teaching_lessons_listeners)
                    listener.teachingLessonUpdated(l_pos, l_ctx);
            }

            @Override
            public void stateChanged(Lesson.LessonState state) {
                for (TeachingLessonsListener listener : teaching_lessons_listeners)
                    listener.teachingLessonUpdated(l_pos, l_ctx);
            }

            @Override
            public void studentAdded(int pos, StudentContext s_ctx) {
                for (TeachingLessonsListener listener : teaching_lessons_listeners)
                    listener.teachingLessonUpdated(l_pos, l_ctx);
            }

            @Override
            public void studentRemoved(int pos, StudentContext s_ctx) {
                for (TeachingLessonsListener listener : teaching_lessons_listeners)
                    listener.teachingLessonUpdated(l_pos, l_ctx);
            }

            @Override
            public void addedToken(int pos, TeachingLessonContext.TokenRow tk) {
                for (TeachingLessonsListener listener : teaching_lessons_listeners)
                    listener.teachingLessonUpdated(l_pos, l_ctx);
            }

            @Override
            public void removedToken(int pos, TeachingLessonContext.TokenRow tk) {
                for (TeachingLessonsListener listener : teaching_lessons_listeners)
                    listener.teachingLessonUpdated(l_pos, l_ctx);
            }

            @Override
            public void updatedToken(int pos, TeachingLessonContext.TokenRow tk) {
                for (TeachingLessonsListener listener : teaching_lessons_listeners)
                    listener.teachingLessonUpdated(l_pos, l_ctx);
            }
        };
        l_ctx.addListener(l);
        id_teaching_lesson_listener.put(l_ctx.getLesson().id, l);
        try {
            // we subscribe to the lesson's time..
            mqtt.subscribe(user.id + "/input/lesson-" + l_ctx.getLesson().id + "/time", new IMqttMessageListener() {
                @Override
                public void messageArrived(final String topic, final MqttMessage message) {
                    new Handler(Looper.getMainLooper()).post(new Runnable() {
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
                    new Handler(Looper.getMainLooper()).post(new Runnable() {
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
            listener.teachingLessonAdded(l_pos, l_ctx);
    }

    private void removeTeachingLesson(@NonNull final TeachingLessonContext l_ctx) {
        int pos = teaching_lessons.indexOf(l_ctx);
        teaching_lessons.remove(pos);
        id_teaching_lessons.remove(l_ctx.getLesson().id);
        l_ctx.removeListener(id_teaching_lesson_listener.get(l_ctx.getLesson().id));
        id_teaching_lesson_listener.remove(l_ctx.getLesson().id);
        if (user != null && mqtt.isConnected()) {
            try {
                // we unsubscribe from the lesson's time and state..
                mqtt.unsubscribe(user.id + "/input/lesson-" + l_ctx.getLesson().id + "/time");
                mqtt.unsubscribe(user.id + "/input/lesson-" + l_ctx.getLesson().id + "/state");
            } catch (MqttException ex) {
                Log.w(TAG, "MQTT unsubscription failed..", ex);
            }
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

    void addModel(@NonNull final LessonModel model) {
        models.add(model);
    }

    public TeachingLessonContext getTeachingLesson(final long id) {
        return id_teaching_lessons.get(id);
    }

    public List<TeachingLessonContext> getTeachingLessons() {
        return Collections.unmodifiableList(teaching_lessons);
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
                public void messageArrived(String topic, MqttMessage message) {
                    t_ctx.setOnLine(Boolean.parseBoolean(new String(message.getPayload())));
                }
            });
        } catch (MqttException ex) {
            Log.w(TAG, "MQTT subscription failed..", ex);
        }
        for (TeachersListener listener : teachers_listeners) listener.teacherAdded(pos, t_ctx);
    }

    private void removeTeacher(@NonNull final TeacherContext t_ctx) {
        int pos = teachers.indexOf(t_ctx);
        teachers.remove(pos);
        id_teachers.remove(t_ctx.getTeacher().id);
        t_ctx.removeListener(id_teacher_listener.get(t_ctx.getTeacher().id));
        id_teacher_listener.remove(t_ctx.getTeacher().id);
        try {
            if (mqtt.isConnected()) { // we might be removing teachers as a consequence of a connection loss..
                mqtt.unsubscribe(t_ctx.getTeacher().id + "/output/on-line");
            }
        } catch (MqttException ex) {
            Log.w(TAG, "MQTT unsubscription failed..", ex);
        }
        for (TeachersListener listener : teachers_listeners) listener.teacherRemoved(pos, t_ctx);
    }

    public TeacherContext getTeacher(final long id) {
        return id_teachers.get(id);
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
                    new Handler(Looper.getMainLooper()).post(new Runnable() {
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
        for (StudentsListener listener : students_listeners) listener.studentAdded(pos, s_ctx);
    }

    public List<TeacherContext> getTeachers() {
        return Collections.unmodifiableList(teachers);
    }

    private void removeStudent(@NonNull final StudentContext s_ctx) {
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
        for (StudentsListener listener : students_listeners) listener.studentRemoved(pos, s_ctx);
    }

    public StudentContext getStudent(final long id) {
        return id_students.get(id);
    }

    @SuppressLint("StaticFieldLeak")
    public boolean login(@NonNull final Context ctx, @NonNull final String email, @NonNull final String password) throws ExecutionException, InterruptedException {
        return new AsyncTask<String, Integer, Boolean>() {
            @Override
            protected Boolean doInBackground(String... strings) {
                try {
                    Response<User> login_response = resource.login(strings[0], strings[1]).execute();
                    if (!login_response.isSuccessful())
                        throw new IOException(login_response.errorBody().string());
                    Log.i(TAG, "Login successful..");
                    User user = login_response.body();

                    // we set the parameters of init's user (these parameters will be communicated to the server..)
                    assert user != null;
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

                    // we add the available lesson models..
                    models.addAll(user.models.values());

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

    public void logout(@NonNull final Context ctx) {
        setUser(ctx, null);
    }

    public List<StudentContext> getStudents() {
        return Collections.unmodifiableList(students);
    }

    @SuppressLint("StaticFieldLeak")
    public void addTeachingLesson(@NonNull final Context ctx, @NonNull final String name, @NonNull final LessonModel model) {
        new AsyncTask<Object, Integer, Void>() {
            @Override
            protected Void doInBackground(Object... objects) {
                try {
                    final Response<Lesson> new_lesson_response = (((LessonModel) objects[2]).id == null ?
                            resource.new_lesson((Long) objects[0], (String) objects[1], GSON.toJson(objects[2])) :
                            resource.new_lesson((Long) objects[0], (String) objects[1], ((LessonModel) objects[2]).id)).execute();
                    if (!new_lesson_response.isSuccessful())
                        throw new IOException(new_lesson_response.errorBody().string());
                    final Lesson l = new_lesson_response.body();
                    // we set the model of the returned lesson..
                    l.model = model;
                    // the new lesson has not any tokens yet..
                    l.tokens = new ArrayList<>();
                    // we add a new context for the returned lesson..
                    addTeachingLesson(new TeachingLessonContext(l));
                    // we solve the lesson..
                    final Response<Void> solve_response = resource.solve(l.id).execute();
                    if (!solve_response.isSuccessful())
                        throw new IOException(solve_response.errorBody().string());
                } catch (final IOException e) {
                    Log.w(TAG, "Lesson creation failed..", e);
                    ((Activity) ctx).runOnUiThread(new Runnable() {
                        public void run() {
                            Toast.makeText(ctx, e.getLocalizedMessage(), Toast.LENGTH_LONG).show();
                        }
                    });
                }
                return null;
            }
        }.execute(user.id, name, model);
    }

    @SuppressLint("StaticFieldLeak")
    public void removeTeachingLesson(@NonNull final Context ctx, @NonNull final TeachingLessonContext l_ctx) {
        new AsyncTask<Long, Integer, Void>() {
            @Override
            protected Void doInBackground(Long... longs) {
                try {
                    final Response<Void> delete_lesson_response = resource.delete_lesson(longs[0]).execute();
                    if (!delete_lesson_response.isSuccessful())
                        throw new IOException(delete_lesson_response.errorBody().string());
                    // we remove the context for the lesson..
                    removeTeachingLesson(l_ctx);
                } catch (final IOException e) {
                    Log.w(TAG, "Lesson removal failed..", e);
                    ((Activity) ctx).runOnUiThread(new Runnable() {
                        public void run() {
                            Toast.makeText(ctx, e.getLocalizedMessage(), Toast.LENGTH_LONG).show();
                        }
                    });
                }
                return null;
            }
        }.execute(l_ctx.getLesson().id);
    }

    @SuppressLint("StaticFieldLeak")
    public void followLesson(@NonNull final Context ctx, @NonNull final Lesson lesson, @NonNull final ArrayList<CharSequence> interests) {
        new AsyncTask<Object, Integer, Void>() {
            @Override
            protected Void doInBackground(Object... objects) {
                try {
                    final Response<Lesson> follow_response = resource.follow((long) objects[0], (long) objects[1], GSON.toJson(objects[2])).execute();
                    if (!follow_response.isSuccessful())
                        throw new IOException(follow_response.errorBody().string());
                    addFollowingLesson(new FollowingLessonContext(follow_response.body()));
                } catch (final IOException e) {
                    Log.w(TAG, "Lesson following failed..", e);
                    ((Activity) ctx).runOnUiThread(new Runnable() {
                        public void run() {
                            Toast.makeText(ctx, e.getLocalizedMessage(), Toast.LENGTH_LONG).show();
                        }
                    });
                }
                return null;
            }
        }.execute(user.id, lesson.id, interests);
    }

    @SuppressLint("StaticFieldLeak")
    public void unfollowLesson(@NonNull final Context ctx, @NonNull final FollowingLessonContext l_ctx) {
        new AsyncTask<Object, Integer, Void>() {
            @Override
            protected Void doInBackground(Object... objects) {
                try {
                    final Response<Void> unfollow_response = resource.unfollow((long) objects[0], (long) objects[1]).execute();
                    if (!unfollow_response.isSuccessful())
                        throw new IOException(unfollow_response.errorBody().string());
                    removeFollowingLesson(l_ctx);
                } catch (final IOException e) {
                    Log.w(TAG, "Lesson unfollowing failed..", e);
                    ((Activity) ctx).runOnUiThread(new Runnable() {
                        public void run() {
                            Toast.makeText(ctx, e.getLocalizedMessage(), Toast.LENGTH_LONG).show();
                        }
                    });
                }
                return null;
            }
        }.execute(user.id, l_ctx.getLesson().id);
    }

    @SuppressLint("StaticFieldLeak")
    public Collection<Lesson> getLessons(@NonNull final Context ctx) throws ExecutionException, InterruptedException {
        return new AsyncTask<Void, Integer, Collection<Lesson>>() {
            @Override
            protected Collection<Lesson> doInBackground(Void... voids) {
                try {
                    final Response<Collection<Lesson>> get_lessons_response = resource.get_lessons().execute();
                    if (!get_lessons_response.isSuccessful())
                        throw new IOException(get_lessons_response.errorBody().string());
                    return get_lessons_response.body();
                } catch (final IOException e) {
                    Log.w(TAG, "Lessons retrieval failed..", e);
                    ((Activity) ctx).runOnUiThread(new Runnable() {
                        public void run() {
                            Toast.makeText(ctx, e.getLocalizedMessage(), Toast.LENGTH_LONG).show();
                        }
                    });
                }
                return null;
            }
        }.execute().get();
    }

    @SuppressLint("StaticFieldLeak")
    public void play(@NonNull final Context ctx, Lesson lesson) {
        new AsyncTask<Long, Integer, Void>() {
            @Override
            protected Void doInBackground(Long... longs) {
                try {
                    final Response<Void> play_response = resource.play(longs[0]).execute();
                    if (!play_response.isSuccessful())
                        throw new IOException(play_response.errorBody().string());
                } catch (final IOException e) {
                    Log.w(TAG, "Starting lesson failed..", e);
                    ((Activity) ctx).runOnUiThread(new Runnable() {
                        public void run() {
                            Toast.makeText(ctx, e.getLocalizedMessage(), Toast.LENGTH_LONG).show();
                        }
                    });
                }
                return null;
            }
        }.execute(lesson.id);
    }

    @SuppressLint("StaticFieldLeak")
    public void pause(@NonNull final Context ctx, Lesson lesson) {
        new AsyncTask<Long, Integer, Void>() {
            @Override
            protected Void doInBackground(Long... longs) {
                try {
                    final Response<Void> pause_response = resource.pause(longs[0]).execute();
                    if (!pause_response.isSuccessful())
                        throw new IOException(pause_response.errorBody().string());
                } catch (final IOException e) {
                    Log.w(TAG, "Pausing lesson failed..", e);
                    ((Activity) ctx).runOnUiThread(new Runnable() {
                        public void run() {
                            Toast.makeText(ctx, e.getLocalizedMessage(), Toast.LENGTH_LONG).show();
                        }
                    });
                }
                return null;
            }
        }.execute(lesson.id);
    }

    @SuppressLint("StaticFieldLeak")
    public void stop(@NonNull final Context ctx, Lesson lesson) {
        new AsyncTask<Long, Integer, Void>() {
            @Override
            protected Void doInBackground(Long... longs) {
                try {
                    final Response<Void> stop_response = resource.stop(longs[0]).execute();
                    if (!stop_response.isSuccessful())
                        throw new IOException(stop_response.errorBody().string());
                } catch (final IOException e) {
                    Log.w(TAG, "Stopping lesson failed..", e);
                    ((Activity) ctx).runOnUiThread(new Runnable() {
                        public void run() {
                            Toast.makeText(ctx, e.getLocalizedMessage(), Toast.LENGTH_LONG).show();
                        }
                    });
                }
                return null;
            }
        }.execute(lesson.id);
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

    @SuppressLint("StaticFieldLeak")
    public boolean newUser(@NonNull final Context ctx, @NonNull final String email, @NonNull final String password, @NonNull final String first_name, @NonNull final String last_name) throws ExecutionException, InterruptedException {
        return new AsyncTask<String, Integer, Boolean>() {
            @Override
            protected Boolean doInBackground(String... strings) {
                try {
                    Response<User> new_user_response = resource.new_user(strings[0], strings[1], strings[2], strings[3]).execute();
                    if (!new_user_response.isSuccessful())
                        throw new IOException(new_user_response.errorBody().string());
                    Log.i(TAG, "Login successful..");
                    User user = new_user_response.body();

                    // we set the parameters of init's user (these parameters will be communicated to the server..)
                    user.par_types = get_par_types(ctx);
                    user.par_values = get_par_values(ctx);

                    setUser(ctx, user);

                    return true;
                } catch (final IOException e) {
                    Log.w(TAG, "User creation failed..", e);
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
}
