/*
 * Copyright (C) 2018 Riccardo De Benedictis
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package it.cnr.istc.pst.exploraa.api;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import javax.json.Json;
import javax.json.JsonArrayBuilder;
import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;
import javax.json.JsonString;
import javax.json.JsonValue;
import javax.json.bind.adapter.JsonbAdapter;

/**
 *
 * @author Riccardo De Benedictis
 */
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

    public User(long id, String email, String first_name, String last_name, boolean online, Map<String, Parameter> par_types, Map<String, Map<String, String>> par_values, Map<Long, Follow> follows, Map<Long, Teach> teachs, Map<Long, LessonModel> models) {
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

    public static class UserAdapter implements JsonbAdapter<User, JsonObject> {

        @Override
        public JsonObject adaptToJson(User obj) throws Exception {
            JsonObjectBuilder user_builder = Json.createObjectBuilder();
            user_builder.add("id", obj.id);
            user_builder.add("email", obj.email);
            user_builder.add("first_name", obj.first_name);
            user_builder.add("last_name", obj.last_name);
            user_builder.add("online", obj.online);
            JsonArrayBuilder par_types_builder = Json.createArrayBuilder();
            for (Parameter par_type : obj.par_types.values()) {
                par_types_builder.add(Parameter.ADAPTER.adaptToJson(par_type));
            }
            user_builder.add("par_types", par_types_builder);
            JsonObjectBuilder par_values_builder = Json.createObjectBuilder();
            for (Map.Entry<String, Map<String, String>> entry : obj.par_values.entrySet()) {
                JsonObjectBuilder par_val_builder = Json.createObjectBuilder();
                for (Map.Entry<String, String> sub_par_val : entry.getValue().entrySet()) {
                    par_val_builder.add(sub_par_val.getKey(), sub_par_val.getValue());
                }
                par_values_builder.add(entry.getKey(), par_val_builder);
            }
            user_builder.add("par_values", par_values_builder);
            if (obj.follows != null) {
                JsonArrayBuilder follows_builder = Json.createArrayBuilder();
                for (Follow follow : obj.follows.values()) {
                    JsonObjectBuilder follow_builder = Json.createObjectBuilder();
                    follow_builder.add("lesson", Lesson.ADAPTER.adaptToJson(follow.lesson));
                    JsonArrayBuilder interests_builder = Json.createArrayBuilder();
                    for (String interest : follow.interests) {
                        interests_builder.add(interest);
                    }
                    user_builder.add("interests", interests_builder);
                    follows_builder.add(follow_builder);
                }
                user_builder.add("follows", follows_builder);
            }
            if (obj.teachs != null) {
                JsonArrayBuilder teachs_builder = Json.createArrayBuilder();
                for (Teach teach : obj.teachs.values()) {
                    JsonObjectBuilder teach_builder = Json.createObjectBuilder();
                    teach_builder.add("lesson", Lesson.ADAPTER.adaptToJson(teach.lesson));
                    teachs_builder.add(teach_builder);
                }
                user_builder.add("teachs", teachs_builder);
            }
            if (obj.models != null) {
                JsonArrayBuilder models_builder = Json.createArrayBuilder();
                for (LessonModel model : obj.models.values()) {
                    models_builder.add(LessonModel.ADAPTER.adaptToJson(model));
                }
                user_builder.add("models", models_builder);
            }
            return user_builder.build();
        }

        @Override
        public User adaptFromJson(JsonObject obj) throws Exception {
            int id = obj.getInt("id");
            String email = obj.getString("email");
            String first_name = obj.getString("first_name");
            String last_name = obj.getString("last_name");
            boolean online = obj.getBoolean("online");

            Map<String, Parameter> par_types = new HashMap<>(obj.getJsonArray("par_types").size());
            for (JsonValue par_type : obj.getJsonArray("par_types")) {
                Parameter p = Parameter.ADAPTER.adaptFromJson(par_type.asJsonObject());
                par_types.put(p.name, p);
            }

            Map<String, Map<String, String>> par_values = new HashMap<>();
            for (Map.Entry<String, JsonValue> entry : obj.getJsonObject("par_values").entrySet()) {
                Map<String, String> par_val = new HashMap<>();
                for (Map.Entry<String, JsonValue> sub_par_val : entry.getValue().asJsonObject().entrySet()) {
                    par_val.put(sub_par_val.getKey(), ((JsonString) sub_par_val.getValue()).getString());
                }
                par_values.put(entry.getKey(), par_val);
            }

            Map<Long, Follow> follows = null;
            if (obj.containsKey("follows")) {
                follows = new HashMap<>(obj.getJsonArray("follows").size());
                for (JsonValue follow : obj.getJsonArray("follows")) {
                    JsonObject follow_object = follow.asJsonObject();
                    Lesson l = Lesson.ADAPTER.adaptFromJson(follow_object.getJsonObject("lesson"));
                    Set<String> interests = new HashSet<>(obj.getJsonArray("interests").size());
                    for (JsonValue interest : obj.getJsonArray("interests")) {
                        interests.add(((JsonString) interest).getString());
                    }
                    follows.put(l.id, new Follow(null, l, interests));
                }
            }

            Map<Long, Teach> teachs = null;
            if (obj.containsKey("teachs")) {
                teachs = new HashMap<>(obj.getJsonArray("teachs").size());
                for (JsonValue teach : obj.getJsonArray("teachs")) {
                    JsonObject teach_object = teach.asJsonObject();
                    Lesson l = Lesson.ADAPTER.adaptFromJson(teach_object.getJsonObject("lesson"));
                    teachs.put(l.id, new Teach(null, l));
                }
            }

            Map<Long, LessonModel> models = null;
            if (obj.containsKey("models")) {
                models = new HashMap<>(obj.getJsonArray("models").size());
                for (JsonValue model : obj.getJsonArray("models")) {
                    LessonModel m = LessonModel.ADAPTER.adaptFromJson(model.asJsonObject());
                    models.put(m.id, m);
                }
            }

            return new User(id, email, first_name, last_name, online, par_types, par_values, follows, teachs, models);
        }
    }
}
