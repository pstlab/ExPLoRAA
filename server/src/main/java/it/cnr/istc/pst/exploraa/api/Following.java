package it.cnr.istc.pst.exploraa.api;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

public class Following {

    private final User student;
    private final User teacher;

    @JsonCreator
    public Following(@JsonProperty("student") User student, @JsonProperty("teacher") User teacher) {
        this.student = student;
        this.teacher = teacher;
    }

    /**
     * @return the student
     */
    public User getStudent() {
        return student;
    }

    /**
     * @return the teacher
     */
    public User getTeacher() {
        return teacher;
    }
}
