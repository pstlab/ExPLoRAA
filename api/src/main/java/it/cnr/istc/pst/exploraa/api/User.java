package it.cnr.istc.pst.exploraa.api;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.Collection;
import java.util.Collections;
import java.util.Map;

/**
 * User
 */
public class User {

    private final long id;
    private final String email;
    private final String first_name;
    private final String last_name;
    private final Map<String, Parameter> par_types;
    private final Map<String, Map<String, String>> par_values;
    private final Collection<Following> following;
    private final Collection<Teaching> teaching;
    private final boolean online;

    @JsonCreator
    public User(@JsonProperty("id") long id, @JsonProperty("email") String email,
                @JsonProperty("firstName") String first_name, @JsonProperty("lastName") String last_name,
                @JsonProperty("parameterTypes") Map<String, Parameter> par_types,
                @JsonProperty("parameterValues") Map<String, Map<String, String>> par_values,
                @JsonProperty("followingLessons") Collection<Following> following,
                @JsonProperty("teachingLessons") Collection<Teaching> teaching, @JsonProperty("online") boolean online) {
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
    public Collection<Following> getFollowingLessons() {
        if (following == null)
            return null;
        return Collections.unmodifiableCollection(following);
    }

    /**
     * @return the teaching
     */
    public Collection<Teaching> getTeachingLessons() {
        if (teaching == null)
            return null;
        return Collections.unmodifiableCollection(teaching);
    }
}