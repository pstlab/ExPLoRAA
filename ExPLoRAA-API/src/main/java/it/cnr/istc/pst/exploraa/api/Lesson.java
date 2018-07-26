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

import java.util.Collection;
import java.util.Map;
import java.util.Set;
import javax.json.Json;
import javax.json.JsonArrayBuilder;
import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;
import javax.json.bind.adapter.JsonbAdapter;

/**
 *
 * @author Riccardo De Benedictis
 */
public class Lesson {

    public static final LessonAdapter ADAPTER = new LessonAdapter();
    public long id;
    public String name;
    public LessonModel model;
    public Set<String> topics;
    public Collection<Message.Stimulus> stimuli;
    public Collection<Message.Token> tokens;
    public Teach teacher;
    public Map<Long, Follow> students;
    public LessonState state;
    public long time;

    public Lesson() {
    }

    public Lesson(long id, String name, LessonModel model, Set<String> topics, Collection<Message.Stimulus> stimuli, Collection<Message.Token> tokens, Teach teacher, Map<Long, Follow> students, LessonState state, long time) {
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

    public static class LessonAdapter implements JsonbAdapter<Lesson, JsonObject> {

        @Override
        public JsonObject adaptToJson(Lesson obj) throws Exception {
            JsonObjectBuilder lesson_builder = Json.createObjectBuilder();
            lesson_builder.add("id", obj.id);
            lesson_builder.add("name", obj.name);

            if (obj.model != null) {
                lesson_builder.add("model", LessonModel.ADAPTER.adaptToJson(obj.model));
            }

            JsonArrayBuilder topics_builder = Json.createArrayBuilder();
            for (String topic : obj.topics) {
                topics_builder.add(topic);
            }
            lesson_builder.add("topics", topics_builder);

            if (obj.stimuli != null) {
                JsonArrayBuilder stimuli_builder = Json.createArrayBuilder();
                for (Message.Stimulus stimulus : obj.stimuli) {
                    stimuli_builder.add(Message.ADAPTER.adaptToJson(stimulus));
                }
                lesson_builder.add("stimuli", stimuli_builder);
            }
            if (obj.tokens != null) {
                JsonArrayBuilder stimuli_builder = Json.createArrayBuilder();
                for (Message.Token token : obj.tokens) {
                    stimuli_builder.add(Message.ADAPTER.adaptToJson(token));
                }
                lesson_builder.add("tokens", stimuli_builder);
            }

            JsonObjectBuilder teacher_builder = Json.createObjectBuilder();
            if (obj.teacher.user != null) {
                teacher_builder.add("user", User.ADAPTER.adaptToJson(obj.teacher.user));
            }
            if (obj.teacher.lesson != null) {
                teacher_builder.add("lesson", Lesson.ADAPTER.adaptToJson(obj.teacher.lesson));
            }
            lesson_builder.add("teacher", teacher_builder);

            if (obj.students != null) {
                JsonArrayBuilder students_builder = Json.createArrayBuilder();
                for (Follow follow : obj.students.values()) {
                    JsonObjectBuilder follow_builder = Json.createObjectBuilder();
                    if (follow.user != null) {
                        follow_builder.add("user", User.ADAPTER.adaptToJson(follow.user));
                    }
                    if (follow.lesson != null) {
                        follow_builder.add("lesson", Lesson.ADAPTER.adaptToJson(follow.lesson));
                    }
                    JsonArrayBuilder interests_builder = Json.createArrayBuilder();
                    for (String interest : follow.interests) {
                        interests_builder.add(interest);
                    }
                    follow_builder.add("interests", interests_builder);
                    students_builder.add(follow_builder);
                }
                lesson_builder.add("students", students_builder);
            }

            lesson_builder.add("state", obj.state.name());
            lesson_builder.add("time", obj.time);
            return lesson_builder.build();
        }

        @Override
        public Lesson adaptFromJson(JsonObject obj) throws Exception {
            throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
        }
    }
}
