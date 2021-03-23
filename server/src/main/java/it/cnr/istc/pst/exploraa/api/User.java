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
    private final String profile;
    private final Map<Long, User> teachers;
    private final Map<Long, User> students;
    private final Map<Long, Lesson> following_lesson;
    private final Map<Long, Lesson> teaching_lesson;
    private final boolean online;

    @JsonCreator
    public User(@JsonProperty("id") final long id, @JsonProperty("email") final String email,
            @JsonProperty("firstName") final String first_name, @JsonProperty("lastName") final String last_name,
            @JsonProperty("profile") final String profile, @JsonProperty("teachers") final Map<Long, User> teachers,
            @JsonProperty("students") final Map<Long, User> students,
            @JsonProperty("followingLessons") final Map<Long, Lesson> following_lesson,
            @JsonProperty("teachingLessons") final Map<Long, Lesson> teaching_lesson,
            @JsonProperty("online") final boolean online) {
        this.id = id;
        this.email = email;
        this.first_name = first_name;
        this.last_name = last_name;
        this.profile = profile;
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
     * @return the profile
     */
    public String getProfile() {
        return profile;
    }

    public Map<Long, User> getTeachers() {
        if (teachers == null)
            return null;
        return Collections.unmodifiableMap(teachers);
    }

    public Map<Long, User> getStudents() {
        if (students == null)
            return null;
        return Collections.unmodifiableMap(students);
    }

    /**
     * @return the following
     */
    public Map<Long, Lesson> getFollowingLessons() {
        if (following_lesson == null)
            return null;
        return Collections.unmodifiableMap(following_lesson);
    }

    /**
     * @return the teaching
     */
    public Map<Long, Lesson> getTeachingLessons() {
        if (teaching_lesson == null)
            return null;
        return Collections.unmodifiableMap(teaching_lesson);
    }

    /**
     * @return the online
     */
    public boolean isOnline() {
        return online;
    }
}
