package it.cnr.istc.pst.exploraa.api;

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
    private boolean online;

    @JsonCreator
    public User(@JsonProperty("id") long id, @JsonProperty("email") String email, @JsonProperty("firstName") String first_name,
            @JsonProperty("lastName") String last_name, @JsonProperty("online") boolean online) {
        this.id = id;
        this.email = email;
        this.first_name = first_name;
        this.last_name = last_name;
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
}