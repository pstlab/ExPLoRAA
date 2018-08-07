package it.cnr.istc.exploraa;

import android.content.Intent;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import it.cnr.istc.exploraa.api.Parameter;
import it.cnr.istc.exploraa.api.User;

public class StudentContext {

    public static final String STUDENT_ONLINE = "Student online";
    public static final String UPDATED_STUDENT = "Student updated";
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
            Intent student_online_intent = new Intent(STUDENT_ONLINE + student.id);
            student_online_intent.putExtra("on_line", on_line);
            service.sendBroadcast(student_online_intent);
            Intent student_updated_intent = new Intent(UPDATED_STUDENT);
            student_updated_intent.putExtra("student", student.id);
            service.sendBroadcast(student_updated_intent);
        }
    }
}
