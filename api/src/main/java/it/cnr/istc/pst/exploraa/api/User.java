package it.cnr.istc.pst.exploraa.api;

import java.util.Collections;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * User
 */
public class User {

    private long id;
    private String email;
    private String first_name;
    private String last_name;
    private Map<String, Parameter> par_types;
    private Map<String, Map<String, String>> par_values;
    private Map<Long, Following> following;
    private Map<Long, Teaching> teaching;
    private boolean online;

    @JsonCreator
    public User(@JsonProperty("id") long id, @JsonProperty("email") String email,
            @JsonProperty("firstName") String first_name, @JsonProperty("lastName") String last_name,
            @JsonProperty("par_types") Map<String, Parameter> par_types,
            @JsonProperty("par_values") Map<String, Map<String, String>> par_values,
            @JsonProperty("following") Map<Long, Following> following,
            @JsonProperty("teaching") Map<Long, Teaching> teaching, @JsonProperty("online") boolean online) {
        this.id = id;
        this.email = email;
        this.first_name = first_name;
        this.last_name = last_name;
        this.par_types = par_types;
        this.par_values = par_values;
        this.following = following;
        this.teaching = teaching;
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

    /**
     * @return the following
     */
    public Map<Long, Following> getFollowingLessons() {
        if (following == null)
            return null;
        return Collections.unmodifiableMap(following);
    }

    /**
     * @return the teaching
     */
    public Map<Long, Teaching> getTeachingLessons() {
        if (teaching == null)
            return null;
        return Collections.unmodifiableMap(teaching);
    }
}