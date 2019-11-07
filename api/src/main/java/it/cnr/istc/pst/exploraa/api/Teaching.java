package it.cnr.istc.pst.exploraa.api;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * Teaching
 */
public class Teaching {

    private User user;
    private Lesson lesson;

    @JsonCreator
    public Teaching(@JsonProperty("user") User user, @JsonProperty("lesson") Lesson lesson) {
        this.user = user;
        this.lesson = lesson;
    }

    /**
     * @return the user
     */
    public User getUser() {
        return user;
    }

    /**
     * @return the lesson
     */
    public Lesson getLesson() {
        return lesson;
    }
}