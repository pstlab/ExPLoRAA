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
import com.google.gson.annotations.JsonAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonToken;
import com.google.gson.stream.JsonWriter;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author Riccardo De Benedictis
 */
public class Lesson {

    public static final LessonAdapter ADAPTER = new LessonAdapter();
    public long id;
    public String name;
    @JsonAdapter(LessonModel.LessonModelAdapter.class)
    public LessonModel model;
    public Set<String> topics;
    @JsonAdapter(Lesson.StimulusListAdapter.class)
    public List<Message.Stimulus> stimuli;
    public List<Message.Token> tokens;
    public Teach teacher;
    public Map<Long, Follow> students;
    public LessonState state;
    public long time;

    public Lesson() {
    }

    public Lesson(long id, String name, LessonModel model, Set<String> topics, List<Message.Stimulus> stimuli, List<Message.Token> tokens, Teach teacher, Map<Long, Follow> students, LessonState state, long time) {
        this.id = id;
        this.name = name;
        this.model = model;
        this.topics = topics;
        this.stimuli = stimuli;
        this.tokens = tokens;
        this.teacher = teacher;
        this.students = students;
        this.state = state;
        this.time = time;
    }

    public enum LessonState {
        Running, Paused, Stopped
    }

    public static class StimulusListAdapter extends TypeAdapter<List<Message.Stimulus>> {

        @Override
        public void write(JsonWriter out, List<Message.Stimulus> value) throws IOException {
            out.beginArray();
            for (Message.Stimulus s : value) Message.ADAPTER.write(out, s);
            out.endArray();
        }

        @Override
        public List<Message.Stimulus> read(JsonReader in) throws IOException {
            List<Message.Stimulus> stimuli = new ArrayList<>();
            in.beginArray();
            while (in.peek() != JsonToken.END_ARRAY)
                stimuli.add((Message.Stimulus) Message.ADAPTER.read(in));
            in.endArray();
            return stimuli;
        }
    }

    public static class LessonAdapter extends TypeAdapter<Lesson> {

        @Override
        public void write(JsonWriter out, Lesson value) throws IOException {
            out.beginObject();
            out.name("id").value(value.id);
            out.name("name").value(value.name);

            if (value.model != null) LessonModel.ADAPTER.write(out.name("model"), value.model);

            out.name("topics");
            out.beginArray();
            for (String topic : value.topics) out.value(topic);
            out.endArray();

            if (value.stimuli != null) {
                out.name("stimuli");
                out.beginArray();
                for (Message.Stimulus stimulus : value.stimuli)
                    Message.ADAPTER.write(out, stimulus);
                out.endArray();
            }
            if (value.tokens != null) {
                out.name("tokens");
                out.beginArray();
                for (Message.Token token : value.tokens) Message.ADAPTER.write(out, token);
                out.endArray();
            }

            out.name("teacher").beginObject();
            if (value.teacher.user != null)
                User.ADAPTER.write(out.name("user"), value.teacher.user);
            if (value.teacher.lesson != null)
                Lesson.ADAPTER.write(out.name("lesson"), value.teacher.lesson);
            out.endObject();

            if (value.students != null) {
                out.name("students");
                out.beginArray();
                for (Follow follow : value.students.values()) {
                    if (follow.user != null) User.ADAPTER.write(out.name("user"), follow.user);
                    if (follow.lesson != null)
                        Lesson.ADAPTER.write(out.name("lesson"), follow.lesson);
                    out.name("interests").beginArray();
                    for (String interest : follow.interests) out.value(interest);
                    out.endArray();
                }
                out.endArray();
            }

            out.name("state").value(value.state.name());
            out.name("time").value(value.time);

            out.endObject();
        }

        @Override
        public Lesson read(JsonReader in) throws IOException {
            final Lesson l = new Lesson();
            in.beginObject();
            while (in.hasNext())
                switch ((in.nextName())) {
                    case "id":
                        l.id = in.nextLong();
                        break;
                    case "model":
                        l.model = LessonModel.ADAPTER.read(in);
                        break;
                    case "topics":
                        l.topics = new HashSet<>();
                        in.beginArray();
                        while (in.peek() != JsonToken.END_ARRAY) l.topics.add(in.nextString());
                        in.endArray();
                        break;
                    case "stimuli":
                        l.stimuli = new ArrayList<>();
                        in.beginArray();
                        while (in.peek() != JsonToken.END_ARRAY)
                            l.stimuli.add((Message.Stimulus) Message.ADAPTER.read(in));
                        in.endArray();
                        break;
                    case "tokens":
                        l.tokens = new ArrayList<>();
                        in.beginArray();
                        while (in.peek() != JsonToken.END_ARRAY)
                            l.tokens.add((Message.Token) Message.ADAPTER.read(in));
                        in.endArray();
                        break;
                    case "teacher":
                        l.teacher = new Teach();
                        in.beginObject();
                        while (in.peek() != JsonToken.END_OBJECT)
                            switch ((in.nextName())) {
                                case "user":
                                    l.teacher.user = User.ADAPTER.read(in);
                                    break;
                                case "lesson":
                                    l.teacher.lesson = Lesson.ADAPTER.read(in);
                                    break;
                            }
                        in.endObject();
                        break;
                    case "students":
                        l.students = new HashMap<>();
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
                            l.students.put(t.id, new Follow(t, c_l, interests));
                        }
                        in.endArray();
                        break;
                    case "state":
                        l.state = LessonState.valueOf(in.nextString());
                        break;
                    case "time":
                        l.time = in.nextLong();
                        break;
                }
            in.endObject();
            return l;
        }
    }
}
