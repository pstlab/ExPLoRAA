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
package it.cnr.istc.exploraa.api;

import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonToken;
import com.google.gson.stream.JsonWriter;

import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
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

    public static class UserAdapter extends TypeAdapter<User> {

        @Override
        public void write(JsonWriter out, User value) throws IOException {
            out.beginObject();
            out.name("id").value(value.id);
            out.name("email").value(value.email);
            out.name("first_name").value(value.first_name);
            out.name("last_name").value(value.last_name);
            out.name("online").value(value.online);

            out.name("par_types");
            out.beginArray();
            for (Parameter par_type : value.par_types.values())
                Parameter.ADAPTER.write(out, par_type);
            out.endArray();

            out.name("par_values");
            out.beginObject();
            for (Map.Entry<String, Map<String, String>> entry : value.par_values.entrySet()) {
                out.name(entry.getKey());
                out.beginObject();
                for (Map.Entry<String, String> sub_par_val : entry.getValue().entrySet())
                    out.name(sub_par_val.getKey()).value(sub_par_val.getValue());
                out.endObject();
            }
            out.endObject();

            if (value.follows != null) {
                out.name("follows");
                out.beginArray();
                for (Follow follow : value.follows.values()) {
                    out.beginObject();
                    if (follow.user != null)
                        User.ADAPTER.write(out.name("user"), follow.user);
                    if (follow.lesson != null)
                        Lesson.ADAPTER.write(out.name("lesson"), follow.lesson);
                    out.name("interests");
                    out.beginArray();
                    for (String interest : follow.interests)
                        out.value(interest);
                    out.endArray();
                    out.endObject();
                }
                out.endArray();
            }
            if (value.teachs != null) {
                out.name("teachs");
                out.beginArray();
                for (Teach teach : value.teachs.values()) {
                    out.beginObject();
                    if (teach.user != null)
                        User.ADAPTER.write(out.name("user"), teach.user);
                    if (teach.lesson != null)
                        Lesson.ADAPTER.write(out.name("lesson"), teach.lesson);
                    out.endObject();
                }
                out.endArray();
            }
            if (value.models != null) {
                out.name("models");
                out.beginArray();
                for (LessonModel model : value.models.values())
                    LessonModel.ADAPTER.write(out, model);
                out.endArray();
            }
            out.endObject();
        }

        @Override
        public User read(JsonReader in) throws IOException {
            User u = new User();
            in.beginObject();
            while (in.hasNext())
                switch ((in.nextName())) {
                    case "id":
                        u.id = in.nextLong();
                        break;
                    case "email":
                        u.email = in.nextString();
                        break;
                    case "first_name":
                        u.first_name = in.nextString();
                        break;
                    case "last_name":
                        u.last_name = in.nextString();
                        break;
                    case "online":
                        u.online = in.nextBoolean();
                        break;
                    case "par_types":
                        u.par_types = new HashMap<>();
                        in.beginArray();
                        while (in.peek() != JsonToken.END_ARRAY) {
                            Parameter p = Parameter.ADAPTER.read(in);
                            u.par_types.put(p.name, p);
                        }
                        in.endArray();
                        break;
                    case "par_values":
                        u.par_values = new HashMap<>();
                        in.beginObject();
                        while (in.peek() != JsonToken.END_OBJECT) {
                            String par_name = in.nextName();
                            Map<String, String> par_vals = new HashMap<>();
                            in.beginObject();
                            while (in.peek() != JsonToken.END_OBJECT) {
                                par_vals.put(in.nextName(), in.nextString());
                            }
                            in.endObject();
                        }
                        in.endObject();
                        break;
                    case "follows":
                        u.follows = new HashMap<>();
                        in.beginArray();
                        while (in.peek() != JsonToken.END_ARRAY) {
                            in.beginObject();
                            User t = null;
                            Lesson c_l = null;
                            Set<String> interests = new HashSet<>();
                            switch ((in.nextName())) {
                                case "user":
                                    t = User.ADAPTER.read(in);
                                    break;
                                case "lesson":
                                    c_l = Lesson.ADAPTER.read(in);
                                    break;
                                case "interests":
                                    in.beginArray();
                                    while (in.peek() != JsonToken.END_ARRAY)
                                        interests.add(in.nextString());
                                    in.endArray();
                            }
                            in.endObject();
                            u.follows.put(c_l.id, new Follow(t, c_l, interests));
                        }
                        in.endArray();
                        break;
                    case "teachs":
                        u.teachs = new HashMap<>();
                        in.beginArray();
                        while (in.peek() != JsonToken.END_ARRAY) {
                            in.beginObject();
                            User s = null;
                            Lesson c_l = null;
                            Set<String> interests = new HashSet<>();
                            switch ((in.nextName())) {
                                case "user":
                                    s = User.ADAPTER.read(in);
                                    break;
                                case "lesson":
                                    c_l = Lesson.ADAPTER.read(in);
                                    break;
                            }
                            in.endObject();
                            u.teachs.put(c_l.id, new Teach(s, c_l));
                        }
                        in.endArray();
                        break;
                    case "models":
                        u.models = new HashMap<>();
                        in.beginArray();
                        while (in.peek() != JsonToken.END_ARRAY) {
                            final LessonModel m = LessonModel.ADAPTER.read(in);
                            u.models.put(m.id, m);
                        }
                        in.endArray();
                        break;
                }
            in.endObject();
            return u;
        }
    }
}
