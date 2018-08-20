package it.cnr.istc.exploraa;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import it.cnr.istc.exploraa.api.Parameter;
import it.cnr.istc.exploraa.api.User;

public class StudentContext {

    private final ExPLoRAAService service;
    private final User student;
    /**
     * The current student's parameter types.
     */
    private final List<Parameter> par_types = new ArrayList<>();
    private final Map<String, Parameter> id_par_types = new HashMap<>();
    /**
     * The current student's parameter values.
     */
    private final Map<String, Map<String, String>> par_vals = new HashMap<>();
    private boolean on_line;
    private final List<StudentListener> listeners = new ArrayList<>();

    StudentContext(ExPLoRAAService service, User student) {
        this.service = service;
        this.student = student;
        this.on_line = student.online;
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
            for (StudentListener l : listeners) l.onlineChanged(on_line);
        }
    }

    public void addListener(StudentListener l) {
        listeners.add(l);
    }

    public void removeListener(StudentListener l) {
        listeners.remove(l);
    }

    public interface StudentListener {

        void onlineChanged(boolean on_line);
    }
}
