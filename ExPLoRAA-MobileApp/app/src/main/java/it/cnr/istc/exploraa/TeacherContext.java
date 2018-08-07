package it.cnr.istc.exploraa;

import android.content.Intent;

import it.cnr.istc.exploraa.api.User;

public class TeacherContext {

    public static final String TEACHER_ONLINE = "Teacher online";
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
            Intent lesson_state_changed_intent = new Intent(TEACHER_ONLINE);
            lesson_state_changed_intent.putExtra("lesson", teacher.id);
            lesson_state_changed_intent.putExtra("on_line", on_line);
            service.sendBroadcast(lesson_state_changed_intent);
        }
    }
}
