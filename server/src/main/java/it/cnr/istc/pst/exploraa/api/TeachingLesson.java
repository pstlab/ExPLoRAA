package it.cnr.istc.pst.exploraa.api;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

public class TeachingLesson {

    private final User user;
    private final Lesson lesson;

    @JsonCreator
    public TeachingLesson(@JsonProperty("user") User user, @JsonProperty("lesson") Lesson lesson) {
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
