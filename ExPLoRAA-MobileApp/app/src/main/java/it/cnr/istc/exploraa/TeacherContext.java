package it.cnr.istc.exploraa;

import android.content.Intent;

import it.cnr.istc.exploraa.api.User;

public class TeacherContext {

    public static final String TEACHER_ONLINE = "Teacher online";
    public static final String UPDATED_TEACHER = "Teacher updated";
    private final ExPLoRAAService service;
    private final User teacher;
    private boolean on_line;

    TeacherContext(ExPLoRAAService service, User teacher) {
        this.service = service;
        this.teacher = teacher;
        this.on_line = teacher.online;
    }

    public User getTeacher() {
        return teacher;
    }

    public boolean isOnLine() {
        return on_line;
    }

    public void setOnLine(boolean on_line) {
        if (this.on_line != on_line) {
            this.on_line = on_line;
            Intent student_online_intent = new Intent(TEACHER_ONLINE + teacher.id);
            student_online_intent.putExtra("on_line", on_line);
            service.sendBroadcast(student_online_intent);
            Intent teacher_updated_intent = new Intent(UPDATED_TEACHER);
            teacher_updated_intent.putExtra("teacher", teacher.id);
            service.sendBroadcast(teacher_updated_intent);
        }
    }
}
