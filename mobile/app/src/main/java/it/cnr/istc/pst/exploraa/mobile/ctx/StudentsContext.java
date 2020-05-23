package it.cnr.istc.pst.exploraa.mobile.ctx;

import android.util.Log;

import androidx.annotation.NonNull;

import org.eclipse.paho.client.mqttv3.MqttException;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import it.cnr.istc.pst.exploraa.api.Parameter;
import it.cnr.istc.pst.exploraa.api.User;

public class StudentsContext {

    private static final StudentsContext instance = new StudentsContext();
    private final Map<Long, StudentContext> students = new HashMap<>();
    private final List<StudentsListener> listeners = new ArrayList<>();

    private StudentsContext() {
        Log.i(StudentsContext.class.getName(), "Creating Students context..");
    }

    public static StudentsContext getInstance() {
        return instance;
    }

    public StudentContext addStudent(@NonNull User student) {
        StudentContext student_ctx = students.get(student.getId());
        if (student_ctx == null) {
            student_ctx = new StudentContext(student);
            students.put(student.getId(), student_ctx);
            try {
                ExPLoRAAContext.getInstance().mqtt.subscribe(Long.toString(student.getId()), (topic, message) -> students.get(student.getId()).setOnLine(Boolean.parseBoolean(new String(message.getPayload()))));
            } catch (MqttException e) {
                Log.e(StudentsContext.class.getName(), "MQTT subscription failed..", e);
            }
            for (StudentsListener l : listeners) l.studentAdded(student_ctx);
        }
        return student_ctx;
    }

    public void removeStudent(long id) {
        StudentContext student_ctx = students.remove(id);
        if (student_ctx != null) {
            try {
                ExPLoRAAContext.getInstance().mqtt.unsubscribe(Long.toString(id));
            } catch (MqttException e) {
                Log.e(StudentsContext.class.getName(), "MQTT unsubscription failed..", e);
            }
            for (StudentsListener l : listeners) l.studentRemoved(student_ctx);
        }
    }

    public StudentContext getStudent(long id) {
        return students.get(id);
    }

    public Collection<StudentContext> getStudents() {
        return students.values();
    }

    public void clear() {
        students.clear();
        for (StudentsListener listener : listeners)
            listener.studentsCleared();
    }

    public void addStudentsListener(@NonNull StudentsListener l) {
        listeners.add(l);
    }

    public void removeStudentsListener(@NonNull StudentsListener l) {
        listeners.remove(l);
    }

    public interface StudentsListener {

        void studentAdded(@NonNull StudentContext student);

        void studentRemoved(@NonNull StudentContext student);

        void studentsCleared();
    }

    public interface StudentListener {

        void online(boolean on_line);
    }

    public static class StudentContext {

        private final User student;
        /**
         * The current student's parameter types.
         */
        private final Map<String, Parameter> id_par_types = new HashMap<>();
        /**
         * The current student's parameter values.
         */
        private final Map<String, Map<String, String>> par_vals = new HashMap<>();
        private final List<StudentListener> listeners = new ArrayList<>();
        private boolean on_line;

        public StudentContext(@NonNull User student) {
            this.student = student;
            id_par_types.putAll(student.getParameterTypes());
            par_vals.putAll(student.getParameterValues());
        }

        public User getStudent() {
            return student;
        }

        public boolean isOnLine() {
            return on_line;
        }

        public void setOnLine(boolean on_line) {
            if (this.on_line != on_line) {
                this.on_line = on_line;
                for (StudentListener l : listeners) l.online(on_line);
            }
        }

        public void addStudentListener(@NonNull StudentListener l) {
            listeners.add(l);
        }

        public void removeStudentListener(@NonNull StudentListener l) {
            listeners.remove(l);
        }
    }
}
