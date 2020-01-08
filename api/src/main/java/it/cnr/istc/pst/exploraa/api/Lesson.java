package it.cnr.istc.pst.exploraa.api;

import java.util.Collections;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * Lesson
 */
public class Lesson {

    private final long id;
    private final String name;
    private final long lesson_id;
    private final Teaching teacher;
    private final Map<Long, Following> students;

    @JsonCreator
    public Lesson(@JsonProperty("id") long id, @JsonProperty("name") String name,
            @JsonProperty("lesson_id") long lesson_id, @JsonProperty("teacher") Teaching teacher,
            @JsonProperty("students") Map<Long, Following> students) {
        this.id = id;
        this.name = name;
        this.lesson_id = lesson_id;
        this.teacher = teacher;
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
     * @return the lesson_id
     */
    public long getLessonId() {
        return lesson_id;
    }

    /**
     * @return the teacher
     */
    public Teaching getTeacher() {
        return teacher;
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