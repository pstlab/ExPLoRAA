package it.cnr.istc.pst.exploraa.api;

/**
 * User
 */
public class User {

    private long id;
    private String email;
    private String first_name;
    private String last_name;
    private boolean online;

    public User(long id, String email, String first_name, String last_name, boolean online) {
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