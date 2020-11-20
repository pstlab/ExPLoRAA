package it.cnr.istc.pst.exploraa.api;

import java.util.Collections;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

public class User {

    private final long id;
    private final String email;
    private final String first_name;
    private final String last_name;
    private final Map<String, Parameter> par_types;
    private final Map<String, Map<String, String>> par_values;
    private final Map<Long, Following> teachers;
    private final Map<Long, Following> students;
    private final Map<Long, FollowingLesson> following_lesson;
    private final Map<Long, TeachingLesson> teaching_lesson;
    private final boolean online;

    @JsonCreator
    public User(@JsonProperty("id") long id, @JsonProperty("email") String email,
            @JsonProperty("firstName") String first_name, @JsonProperty("lastName") String last_name,
            @JsonProperty("parameterTypes") Map<String, Parameter> par_types,
            @JsonProperty("parameterValues") Map<String, Map<String, String>> par_values,
            @JsonProperty("teachers") Map<Long, Following> teachers,
            @JsonProperty("students") Map<Long, Following> students,
            @JsonProperty("followingLessons") Map<Long, FollowingLesson> following_lesson,
            @JsonProperty("teachingLessons") Map<Long, TeachingLesson> teaching_lesson,
            @JsonProperty("online") boolean online) {
        this.id = id;
        this.email = email;
        this.first_name = first_name;
        this.last_name = last_name;
        this.par_types = par_types;
        this.par_values = par_values;
        this.teachers = teachers;
        this.students = students;
        this.following_lesson = following_lesson;
        this.teaching_lesson = teaching_lesson;
        this.online = online;
    }

    /**
     * @return the id
     */
    public long getId() {
        return id;
    }

    /**
     * @return the email
     */
    public String getEmail() {
        return email;
    }

    /**
     * @return the first_name
     */
    public String getFirstName() {
        return first_name;
    }

    /**
     * @return the last_name
     */
    public String getLastName() {
        return last_name;
    }

    /**
     * @return the online
     */
    public boolean isOnline() {
        return online;
    }

    /**
     * @return the par_types
     */
    public Map<String, Parameter> getParameterTypes() {
        if (par_types == null)
            return null;
        return Collections.unmodifiableMap(par_types);
    }

    /**
     * @return the par_values
     */
    public Map<String, Map<String, String>> getParameterValues() {
        if (par_values == null)
            return null;
        return Collections.unmodifiableMap(par_values);
    }

    public Map<Long, Following> getTeachers() {
        if (teachers == null)
            return null;
        return Collections.unmodifiableMap(teachers);
    }

    public Map<Long, Following> getStudents() {
        if (students == null)
            return null;
        return Collections.unmodifiableMap(students);
    }

    /**
     * @return the following
     */
    public Map<Long, FollowingLesson> getFollowingLessons() {
        if (following_lesson == null)
            return null;
        return Collections.unmodifiableMap(following_lesson);
    }

    /**
     * @return the teaching
     */
    public Map<Long, TeachingLesson> getTeachingLessons() {
        if (teaching_lesson == null)
            return null;
        return Collections.unmodifiableMap(teaching_lesson);
    }
}
