package it.cnr.istc.exploraa.api;

import java.util.Set;

/**
 * Follow
 */
public class Follow {

    public User user;
    public Lesson lesson;
    public Set<String> interests;

    public Follow() {
    }

    public Follow(User user, Lesson lesson, Set<String> interests) {
        this.user = user;
        this.lesson = lesson;
        this.interests = interests;
    }
}