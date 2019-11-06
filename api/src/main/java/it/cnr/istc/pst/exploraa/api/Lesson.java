package it.cnr.istc.pst.exploraa.api;

import java.util.Collections;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * Lesson
 */
public class Lesson {

    private long id;
    private String name;
    private Map<Long, Following> students;

    @JsonCreator
    public Lesson(@JsonProperty("id") long id, @JsonProperty("name") String name,
            @JsonProperty("students") Map<Long, Following> students) {
        this.id = id;
        this.name = name;
        this.students = students;
    }

    /**
     * @return the id
     */
    public long getId() {
        return id;
    }

    /**
     * @return the name
     */
    public String getName() {
        return name;
    }

    /**
     * @return the students
     */
    public Map<Long, Following> getStudents() {
        if (students == null)
            return null;
        return Collections.unmodifiableMap(students);
    }
}