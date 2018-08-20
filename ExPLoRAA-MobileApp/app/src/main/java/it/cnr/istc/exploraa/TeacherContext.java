package it.cnr.istc.exploraa;

import java.util.ArrayList;
import java.util.List;

import it.cnr.istc.exploraa.api.User;

public class TeacherContext {

    private final ExPLoRAAService service;
    private final User teacher;
    private boolean on_line;
    private final List<TeacherListener> listeners = new ArrayList<>();

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
            for (TeacherListener l : listeners) l.onlineChanged(on_line);
        }
    }

    public void addListener(TeacherListener l) {
        listeners.add(l);
    }

    public void removeListener(TeacherListener l) {
        listeners.remove(l);
    }

    public interface TeacherListener {

        void onlineChanged(boolean on_line);
    }
}
