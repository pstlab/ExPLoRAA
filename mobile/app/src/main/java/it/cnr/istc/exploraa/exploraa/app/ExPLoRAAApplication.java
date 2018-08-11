package it.cnr.istc.exploraa.exploraa.app;

import android.app.Application;

import java.util.ArrayList;
import java.util.Collection;

import it.cnr.istc.exploraa.api.User;

public class ExPLoRAAApplication extends Application {

    private User user;
    private final Collection<ApplicationListener> listeners = new ArrayList<>();

    public User getUser() {
        return user;
    }

    public void setUser(User user) {
        this.user = user;
    }

    public void addListener(ApplicationListener l) {
        listeners.add(l);
    }

    public void removeListener(ApplicationListener l) {
        listeners.remove(l);
    }
}
