package it.cnr.istc.pst.exploraa.api;

import java.util.Collections;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

public class Following {

    private final User user;
    private final Lesson lesson;
    private final Set<String> interests;

    @JsonCreator
    public Following(@JsonProperty("user") User user, @JsonProperty("lesson") Lesson lesson,
            @JsonProperty("interests") Set<String> interests) {
        this.user = user;
        this.lesson = lesson;
        this.interests = interests;
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

    /**
     * @return the interests
     */
    public Set<String> getInterests() {
        if (interests == null)
            return null;
        return Collections.unmodifiableSet(interests);
    }
}
