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

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonArrayBuilder;
import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;
import javax.json.JsonString;
import javax.json.JsonValue;
import javax.json.bind.adapter.JsonbAdapter;
import javax.json.bind.annotation.JsonbTypeAdapter;

/**
 *
 * @author Riccardo De Benedictis
 */
public abstract class Message {

    public static final MessageAdapter ADAPTER = new MessageAdapter();
    public MessageType message_type;

    public Message() {
    }

    public Message(MessageType type) {
        this.message_type = type;
    }

    public enum MessageType {
        NewParameter,
        RemoveParameter,
        FollowLesson,
        UnfollowLesson,
        RemoveLesson,
        Token,
        TokenUpdate,
        RemoveToken,
        Stimulus,
        Answer,
        RemoveStimulus;
    }

    public static class NewParameter extends Message {

        public Parameter parameter;

        public NewParameter() {
        }

        public NewParameter(Parameter parameter) {
            super(MessageType.NewParameter);
            this.parameter = parameter;
        }
    }

    public static class RemoveParameter extends Message {

        public String parameter;

        public RemoveParameter() {
        }

        public RemoveParameter(String parameter) {
            super(MessageType.RemoveParameter);
            this.parameter = parameter;
        }
    }

    public static class FollowLesson extends Message {

        @JsonbTypeAdapter(User.UserAdapter.class)
        public User student;
        public long lesson;
        public Set<String> interests;

        public FollowLesson() {
        }

        public FollowLesson(User student, long lesson, Set<String> interests) {
            super(MessageType.FollowLesson);
            this.student = student;
            this.lesson = lesson;
            this.interests = interests;
        }
    }

    public static class UnfollowLesson extends Message {

        public long student;
        public long lesson;

        public UnfollowLesson() {
        }

        public UnfollowLesson(long student, long lesson) {
            super(MessageType.UnfollowLesson);
            this.student = student;
            this.lesson = lesson;
        }
    }

    public static class RemoveLesson extends Message {

        public long lesson;

        public RemoveLesson() {
        }

        public RemoveLesson(long lesson) {
            super(MessageType.RemoveLesson);
            this.lesson = lesson;
        }
    }

    public static class Token extends Message {

        public long lesson_id;
        public int id;
        public Integer cause;
        public Long min;
        public Long max;
        public long time;
        public String refEvent;

        public Token() {
        }

        public Token(long lesson_id, int id, Integer cause, Long min, Long max, long time, String refEvent) {
            super(MessageType.Token);
            this.lesson_id = lesson_id;
            this.id = id;
            this.cause = cause;
            this.min = min;
            this.max = max;
            this.time = time;
            this.refEvent = refEvent;
        }
    }

    public static class TokenUpdate extends Message {

        public long lesson_id;
        public int id;
        public Long min, max;
        public long time;

        public TokenUpdate() {
        }

        public TokenUpdate(long lesson_id, int id, Long min, Long max, long time) {
            super(MessageType.TokenUpdate);
            this.lesson_id = lesson_id;
            this.id = id;
            this.min = min;
            this.max = max;
            this.time = time;
        }
    }

    public static class RemoveToken extends Message {

        public long lesson_id;
        public int id;

        public RemoveToken() {
        }

        public RemoveToken(long lesson_id, int id) {
            super(MessageType.RemoveToken);
            this.lesson_id = lesson_id;
            this.id = id;
        }
    }

    public abstract static class Stimulus extends Message {

        public StimulusType stimulus_type;
        public long lesson_id;
        public int id;
        public long time;

        public Stimulus() {
        }

        public Stimulus(StimulusType stimulus_type, long lesson_id, int id, long time) {
            super(MessageType.Stimulus);
            this.stimulus_type = stimulus_type;
            this.lesson_id = lesson_id;
            this.id = id;
            this.time = time;
        }

        public enum StimulusType {
            Text, Question, URL
        }

        public static class TextStimulus extends Stimulus {

            public String content;

            public TextStimulus() {
            }

            public TextStimulus(long lesson_id, int id, long time, String content) {
                super(StimulusType.Text, lesson_id, id, time);
                this.content = content;
            }
        }

        public static class QuestionStimulus extends Stimulus {

            public String question;
            public List<String> answers;
            public Integer answer;

            public QuestionStimulus() {
            }

            public QuestionStimulus(long lesson_id, int id, long time, String question, List<String> answers, Integer answer) {
                super(StimulusType.Question, lesson_id, id, time);
                this.question = question;
                this.answers = answers;
                this.answer = answer;
            }

            public static class Answer extends Message {

                public long lesson_id;
                public int question_id;
                public int answer;

                public Answer() {
                }

                public Answer(long lesson_id, int question_id, int answer) {
                    super(MessageType.Answer);
                    this.lesson_id = lesson_id;
                    this.question_id = question_id;
                    this.answer = answer;
                }
            }
        }

        public static class URLStimulus extends Stimulus {

            public String content;
            public String url;

            public URLStimulus() {
            }

            public URLStimulus(long lesson_id, int id, long time, String content, String url) {
                super(StimulusType.URL, lesson_id, id, time);
                this.content = content;
                this.url = url;
            }
        }
    }

    public static class RemoveStimulus extends Message {

        public long lesson_id;
        public long id;

        public RemoveStimulus() {
        }

        public RemoveStimulus(long lesson_id, long id) {
            super(MessageType.RemoveStimulus);
            this.lesson_id = lesson_id;
            this.id = id;
        }
    }

    public static class MessageAdapter implements JsonbAdapter<Message, JsonObject> {

        @Override
        public JsonObject adaptToJson(Message obj) throws Exception {
            JsonObjectBuilder c_object = Json.createObjectBuilder();
            c_object.add("message_type", obj.message_type.name());
            switch (obj.message_type) {
                case NewParameter:
                    c_object.add("parameter", Parameter.ADAPTER.adaptToJson(((NewParameter) obj).parameter));
                    break;
                case RemoveParameter:
                    c_object.add("parameter", ((RemoveParameter) obj).parameter);
                    break;
                case FollowLesson:
                    c_object.add("student", User.ADAPTER.adaptToJson(((FollowLesson) obj).student));
                    c_object.add("lesson", ((FollowLesson) obj).lesson);
                    JsonArrayBuilder interests_builder = Json.createArrayBuilder();
                    for (String interest : ((FollowLesson) obj).interests) {
                        interests_builder.add(interest);
                    }
                    c_object.add("interests", interests_builder);
                    break;
                case UnfollowLesson:
                    c_object.add("student", ((UnfollowLesson) obj).student);
                    c_object.add("lesson", ((UnfollowLesson) obj).lesson);
                    break;
                case RemoveLesson:
                    c_object.add("lesson", ((RemoveLesson) obj).lesson);
                    break;
                case Token:
                    c_object.add("lesson_id", ((Token) obj).lesson_id);
                    c_object.add("id", ((Token) obj).id);
                    if (((Token) obj).cause != null) {
                        c_object.add("cause", ((Token) obj).cause);
                    }
                    if (((Token) obj).min != null) {
                        c_object.add("min", ((Token) obj).min);
                    }
                    if (((Token) obj).max != null) {
                        c_object.add("max", ((Token) obj).max);
                    }
                    c_object.add("time", ((Token) obj).time);
                    if (((Token) obj).refEvent != null) {
                        c_object.add("refEvent", ((Token) obj).refEvent);
                    }
                    break;
                case TokenUpdate:
                    c_object.add("lesson_id", ((TokenUpdate) obj).lesson_id);
                    c_object.add("id", ((TokenUpdate) obj).id);
                    if (((TokenUpdate) obj).min != null) {
                        c_object.add("min", ((TokenUpdate) obj).min);
                    }
                    if (((TokenUpdate) obj).max != null) {
                        c_object.add("max", ((TokenUpdate) obj).max);
                    }
                    c_object.add("time", ((TokenUpdate) obj).time);
                    break;
                case RemoveToken:
                    c_object.add("lesson_id", ((RemoveToken) obj).lesson_id);
                    c_object.add("id", ((RemoveToken) obj).id);
                    break;
                case Stimulus:
                    Stimulus st = (Stimulus) obj;
                    c_object.add("stimulus_type", st.stimulus_type.name());
                    c_object.add("lesson_id", st.lesson_id);
                    c_object.add("id", st.id);
                    c_object.add("time", st.time);
                    switch (st.stimulus_type) {
                        case Text:
                            c_object.add("content", ((Stimulus.TextStimulus) st).content);
                            break;
                        case Question:
                            c_object.add("question", ((Stimulus.QuestionStimulus) st).question);
                            JsonArrayBuilder answers_builder = Json.createArrayBuilder();
                            for (String answer : ((Stimulus.QuestionStimulus) st).answers) {
                                answers_builder.add(answer);
                            }
                            c_object.add("answers", answers_builder);
                            if (((Stimulus.QuestionStimulus) st).answer != null) {
                                c_object.add("answer", ((Stimulus.QuestionStimulus) st).answer);
                            }
                            break;
                        case URL:
                            c_object.add("content", ((Stimulus.URLStimulus) st).content);
                            c_object.add("url", ((Stimulus.URLStimulus) st).url);
                            break;
                        default:
                            throw new AssertionError(st.stimulus_type.name());
                    }
                    break;
                case Answer:
                    c_object.add("lesson_id", ((Stimulus.QuestionStimulus.Answer) obj).lesson_id);
                    c_object.add("question_id", ((Stimulus.QuestionStimulus.Answer) obj).question_id);
                    c_object.add("answer", ((Stimulus.QuestionStimulus.Answer) obj).answer);
                    break;
                case RemoveStimulus:
                    c_object.add("lesson_id", ((RemoveStimulus) obj).lesson_id);
                    c_object.add("id", ((RemoveStimulus) obj).id);
                    break;
                default:
                    throw new AssertionError(obj.message_type.name());
            }
            return c_object.build();
        }

        @Override
        public Message adaptFromJson(JsonObject obj) throws Exception {
            switch (MessageType.valueOf(obj.getString("message_type"))) {
                case NewParameter:
                    return new NewParameter(Parameter.ADAPTER.adaptFromJson(obj.getJsonObject("parameter")));
                case RemoveParameter:
                    return new RemoveParameter(obj.getString("parameter"));
                case FollowLesson:
                    JsonArray interests_array = obj.getJsonArray("interests");
                    Set<String> interests = new HashSet<>(interests_array.size());
                    for (JsonValue answer_value : interests_array) {
                        interests.add(((JsonString) answer_value).getString());
                    }
                    return new FollowLesson(User.ADAPTER.adaptFromJson(obj.getJsonObject("student")), obj.getInt("lesson"), interests);
                case UnfollowLesson:
                    return new UnfollowLesson(obj.getInt("student"), obj.getInt("lesson"));
                case RemoveLesson:
                    return new RemoveLesson(obj.getInt("lesson"));
                case Token:
                    return new Token(obj.getInt("lesson_id"), obj.getInt("id"), obj.containsKey("cause") ? obj.getInt("cause") : null, obj.containsKey("min") ? new Long(obj.getInt("min")) : null, obj.containsKey("max") ? new Long(obj.getInt("max")) : null, obj.getInt("time"), obj.containsKey("refEvent") ? obj.getString("refEvent") : null);
                case TokenUpdate:
                    return new TokenUpdate(obj.getInt("lesson_id"), obj.getInt("id"), obj.containsKey("min") ? new Long(obj.getInt("min")) : null, obj.containsKey("max") ? new Long(obj.getInt("max")) : null, obj.getInt("time"));
                case RemoveToken:
                    return new RemoveToken(obj.getInt("lesson_id"), obj.getInt("id"));
                case Stimulus:
                    long lesson_id = obj.getJsonNumber("lesson_id").longValue();
                    int id = obj.getInt("id");
                    long time = obj.getJsonNumber("time").longValue();
                    switch (Stimulus.StimulusType.valueOf(obj.getString("stimulus_type"))) {
                        case Text:
                            return new Stimulus.TextStimulus(lesson_id, id, time, obj.getString("content"));
                        case Question:
                            String question = obj.getString("question");
                            JsonArray answers_array = obj.getJsonArray("answers");
                            List<String> answers = new ArrayList<>(answers_array.size());
                            for (JsonValue answer_value : answers_array) {
                                answers.add(((JsonString) answer_value).getString());
                            }
                            return new Stimulus.QuestionStimulus(lesson_id, id, time, question, answers, obj.containsKey("answer") ? obj.getInt("answer") : null);
                        case URL:
                            return new Stimulus.URLStimulus(lesson_id, id, time, obj.getString("content"), obj.getString("url"));
                        default:
                            throw new AssertionError(MessageType.valueOf(obj.getString("message_type")).name());
                    }
                case Answer:
                    return new Stimulus.QuestionStimulus.Answer(obj.getInt("lesson_id"), obj.getInt("question_id"), obj.getInt("answer"));
                case RemoveStimulus:
                    return new RemoveStimulus(obj.getInt("lesson_id"), obj.getInt("id"));
                default:
                    throw new AssertionError(MessageType.valueOf(obj.getString("message_type")).name());
            }
        }
    }
}
