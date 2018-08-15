package it.cnr.istc.exploraa.api;

import java.util.Map;

import com.google.gson.annotations.JsonAdapter;

import it.cnr.istc.exploraa.api.adapters.UserAdapter;

/**
 * This class represents the ExPLoRAA user.
 */
@JsonAdapter(UserAdapter.class)
public class User {

    public static final UserAdapter ADAPTER = new UserAdapter();
    public long id;
    public String email;
    public String first_name;
    public String last_name;
    public boolean online;
    public Map<String, Parameter> par_types;
    public Map<String, Map<String, String>> par_values;
    public Map<Long, Follow> follows;
    public Map<Long, Teach> teachs;
    public Map<Long, LessonModel> models;

    public User() {
    }

    public User(long id, String email, String first_name, String last_name, boolean online,
            Map<String, Parameter> par_types, Map<String, Map<String, String>> par_values, Map<Long, Follow> follows,
            Map<Long, Teach> teachs, Map<Long, LessonModel> models) {
        this.id = id;
        this.email = email;
        this.first_name = first_name;
        this.last_name = last_name;
        this.online = online;
        this.par_types = par_types;
        this.par_values = par_values;
        this.follows = follows;
        this.teachs = teachs;
        this.models = models;
    }
}