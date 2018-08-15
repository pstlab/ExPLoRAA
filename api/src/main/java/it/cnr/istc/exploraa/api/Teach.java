package it.cnr.istc.exploraa.api;

/**
 * Teach
 */
public class Teach {

    public User user;
    public Lesson lesson;

    public Teach() {
    }

    public Teach(User user, Lesson lesson) {
        this.user = user;
        this.lesson = lesson;
    }
}