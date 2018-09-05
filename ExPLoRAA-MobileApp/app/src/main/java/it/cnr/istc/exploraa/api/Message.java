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
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
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
        RemoveStimulus
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

        @JsonAdapter(User.UserAdapter.class)
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
        public Set<Long> students;
        public long time;

        public Stimulus() {
        }

        public Stimulus(StimulusType stimulus_type, long lesson_id, int id, Set<Long> students, long time) {
            super(MessageType.Stimulus);
            this.stimulus_type = stimulus_type;
            this.lesson_id = lesson_id;
            this.id = id;
            this.students = students;
            this.time = time;
        }

        public enum StimulusType {
            Text, Question, URL
        }

        public static class TextStimulus extends Stimulus {

            public String content;

            public TextStimulus() {
            }

            public TextStimulus(long lesson_id, int id, Set<Long> students, long time, String content) {
                super(StimulusType.Text, lesson_id, id, students, time);
                this.content = content;
            }
        }

        public static class QuestionStimulus extends Stimulus {

            public String question;
            public List<String> answers;
            public Integer answer;

            public QuestionStimulus() {
            }

            public QuestionStimulus(long lesson_id, int id, Set<Long> students, long time, String question, List<String> answers, Integer answer) {
                super(StimulusType.Question, lesson_id, id, students, time);
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

            public URLStimulus(long lesson_id, int id, Set<Long> students, long time, String content, String url) {
                super(StimulusType.URL, lesson_id, id, students, time);
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

    public static class MessageAdapter extends TypeAdapter<Message> {

        @Override
        public void write(JsonWriter out, Message value) throws IOException {
            out.beginObject();
            out.name("message_type").value(value.message_type.name());
            switch (value.message_type) {
                case NewParameter:
                    Parameter.ADAPTER.write(out.name("parameter"), ((NewParameter) value).parameter);
                    break;
                case RemoveParameter:
                    out.name("parameter").value(((RemoveParameter) value).parameter);
                    break;
                case FollowLesson:
                    out.name("student");
                    User.ADAPTER.write(out.name("student"), ((FollowLesson) value).student);
                    out.name("lesson").value(((FollowLesson) value).lesson);
                    out.name("interests");
                    out.beginArray();
                    for (String interest : ((FollowLesson) value).interests)
                        out.value(interest);
                    out.endArray();
                    break;
                case UnfollowLesson:
                    out.name("student").value(((UnfollowLesson) value).student);
                    out.name("lesson").value(((UnfollowLesson) value).lesson);
                    break;
                case RemoveLesson:
                    out.name("lesson").value(((RemoveLesson) value).lesson);
                    break;
                case Token:
                    out.name("lesson_id").value(((Token) value).lesson_id);
                    out.name("id").value(((Token) value).id);
                    if (((Token) value).cause != null) {
                        out.name("cause").value(((Token) value).cause);
                    }
                    if (((Token) value).min != null) {
                        out.name("min").value(((Token) value).min);
                    }
                    if (((Token) value).max != null) {
                        out.name("max").value(((Token) value).max);
                    }
                    out.name("time").value(((Token) value).time);
                    if (((Token) value).refEvent != null) {
                        out.name("refEvent").value(((Token) value).refEvent);
                    }
                    break;
                case TokenUpdate:
                    out.name("lesson_id").value(((TokenUpdate) value).lesson_id);
                    out.name("id").value(((TokenUpdate) value).id);
                    if (((TokenUpdate) value).min != null) {
                        out.name("min").value(((TokenUpdate) value).min);
                    }
                    if (((TokenUpdate) value).max != null) {
                        out.name("max").value(((TokenUpdate) value).max);
                    }
                    out.name("time").value(((TokenUpdate) value).time);
                    break;
                case RemoveToken:
                    out.name("lesson_id").value(((RemoveToken) value).lesson_id);
                    out.name("id").value(((RemoveToken) value).id);
                    break;
                case Stimulus:
                    Stimulus st = (Stimulus) value;
                    out.name("stimulus_type").value(st.stimulus_type.name());
                    out.name("lesson_id").value(st.lesson_id);
                    out.name("id").value(st.id);
                    out.name("students");
                    out.beginArray();
                    for (Long student : st.students)
                        out.value(student);
                    out.endArray();
                    out.name("time").value(st.time);
                    switch (st.stimulus_type) {
                        case Text:
                            out.name("content").value(((Stimulus.TextStimulus) st).content);
                            break;
                        case Question:
                            out.name("question").value(((Stimulus.QuestionStimulus) st).question);
                            out.name("answers");
                            out.beginArray();
                            for (String answer : ((Stimulus.QuestionStimulus) st).answers)
                                out.value(answer);
                            out.endArray();
                            if (((Stimulus.QuestionStimulus) st).answer != null)
                                out.name("answer").value(((Stimulus.QuestionStimulus) st).answer);
                            break;
                        case URL:
                            out.name("content").value(((Stimulus.URLStimulus) st).content);
                            out.name("url").value(((Stimulus.URLStimulus) st).url);
                            break;
                        default:
                            throw new AssertionError(st.stimulus_type.name());
                    }
                    break;
                case Answer:
                    out.name("lesson_id").value(((Stimulus.QuestionStimulus.Answer) value).lesson_id);
                    out.name("question_id").value(((Stimulus.QuestionStimulus.Answer) value).question_id);
                    out.name("answer").value(((Stimulus.QuestionStimulus.Answer) value).answer);
                    break;
                case RemoveStimulus:
                    out.name("lesson_id").value(((RemoveStimulus) value).lesson_id);
                    out.name("id").value(((RemoveStimulus) value).id);
                    break;
            }
            out.endObject();
        }

        @Override
        public Message read(JsonReader in) throws IOException {
            in.beginObject();
            in.nextName();
            Message m = null;
            switch (MessageType.valueOf(in.nextString())) {
                case NewParameter:
                    in.nextName();
                    m = new NewParameter(Parameter.ADAPTER.read(in));
                    break;
                case RemoveParameter:
                    in.nextName();
                    m = new RemoveParameter(in.nextString());
                    break;
                case FollowLesson:
                    m = new FollowLesson();
                    m.message_type = MessageType.FollowLesson;
                    while (in.hasNext())
                        switch (in.nextName()) {
                            case "interests":
                                ((FollowLesson) m).interests = new HashSet<>();
                                in.beginArray();
                                while (in.peek() != JsonToken.END_ARRAY)
                                    ((FollowLesson) m).interests.add(in.nextString());
                                in.endArray();
                                break;
                            case "student":
                                ((FollowLesson) m).student = User.ADAPTER.read(in);
                                break;
                            case "lesson":
                                ((FollowLesson) m).lesson = in.nextLong();
                                break;
                        }
                    break;
                case UnfollowLesson:
                    m = new UnfollowLesson();
                    m.message_type = MessageType.UnfollowLesson;
                    while (in.hasNext())
                        switch (in.nextName()) {
                            case "student":
                                ((UnfollowLesson) m).student = in.nextLong();
                                break;
                            case "lesson":
                                ((UnfollowLesson) m).lesson = in.nextLong();
                                break;
                        }
                    break;
                case RemoveLesson:
                    m = new RemoveLesson();
                    m.message_type = MessageType.RemoveLesson;
                    in.nextName();
                    ((RemoveLesson) m).lesson = in.nextLong();
                    break;
                case Token:
                    m = new Token();
                    m.message_type = MessageType.Token;
                    while (in.hasNext())
                        switch (in.nextName()) {
                            case "lesson_id":
                                ((Token) m).lesson_id = in.nextLong();
                                break;
                            case "id":
                                ((Token) m).id = in.nextInt();
                                break;
                            case "cause":
                                ((Token) m).cause = in.nextInt();
                                break;
                            case "min":
                                ((Token) m).min = in.nextLong();
                                break;
                            case "max":
                                ((Token) m).max = in.nextLong();
                                break;
                            case "time":
                                ((Token) m).time = in.nextLong();
                                break;
                            case "refEvent":
                                ((Token) m).refEvent = in.nextString();
                                break;
                        }
                    break;
                case TokenUpdate:
                    m = new TokenUpdate();
                    m.message_type = MessageType.TokenUpdate;
                    while (in.hasNext())
                        switch (in.nextName()) {
                            case "lesson_id":
                                ((TokenUpdate) m).lesson_id = in.nextLong();
                                break;
                            case "id":
                                ((TokenUpdate) m).id = in.nextInt();
                                break;
                            case "min":
                                ((TokenUpdate) m).min = in.nextLong();
                                break;
                            case "max":
                                ((TokenUpdate) m).max = in.nextLong();
                                break;
                            case "time":
                                ((TokenUpdate) m).time = in.nextLong();
                                break;
                        }
                    break;
                case RemoveToken:
                    m = new RemoveToken();
                    m.message_type = MessageType.RemoveToken;
                    while (in.hasNext())
                        switch (in.nextName()) {
                            case "lesson_id":
                                ((RemoveToken) m).lesson_id = in.nextLong();
                                break;
                            case "id":
                                ((RemoveToken) m).id = in.nextInt();
                                break;
                        }
                    break;
                case Stimulus:
                    Stimulus st = null;
                    while (in.hasNext()) {
                        switch (in.nextName()) {
                            case "lesson_id":
                                st.lesson_id = in.nextLong();
                                break;
                            case "id":
                                st.id = in.nextInt();
                                break;
                            case "students":
                                st.students = new HashSet<>();
                                in.beginArray();
                                while (in.peek() != JsonToken.END_ARRAY)
                                    st.students.add(in.nextLong());
                                in.endArray();
                                break;
                            case "time":
                                st.time = in.nextLong();
                                break;
                            case "content":
                                switch (st.stimulus_type) {
                                    case Text:
                                        ((Stimulus.TextStimulus) st).content = in.nextString();
                                        break;
                                    case URL:
                                        ((Stimulus.URLStimulus) st).content = in.nextString();
                                        break;
                                }
                                break;
                            case "question":
                                ((Stimulus.QuestionStimulus) st).question = in.nextString();
                                break;
                            case "answers":
                                in.beginArray();
                                while (in.peek() != JsonToken.END_ARRAY)
                                    ((Stimulus.QuestionStimulus) st).answers.add(in.nextString());
                                in.endArray();
                                break;
                            case "answer":
                                ((Stimulus.QuestionStimulus) st).answer = in.nextInt();
                                break;
                            case "url":
                                ((Stimulus.URLStimulus) st).url = in.nextString();
                                break;
                            case "stimulus_type":
                                switch (Stimulus.StimulusType.valueOf(in.nextString())) {
                                    case Text:
                                        st = new Stimulus.TextStimulus();
                                        st.message_type = MessageType.Stimulus;
                                        ((Stimulus.TextStimulus) st).stimulus_type = Stimulus.StimulusType.Text;
                                        break;
                                    case Question:
                                        st = new Stimulus.QuestionStimulus();
                                        st.message_type = MessageType.Stimulus;
                                        ((Stimulus.QuestionStimulus) st).stimulus_type = Stimulus.StimulusType.Question;
                                        break;
                                    case URL:
                                        st = new Stimulus.URLStimulus();
                                        st.message_type = MessageType.Stimulus;
                                        ((Stimulus.URLStimulus) st).stimulus_type = Stimulus.StimulusType.URL;
                                        break;
                                }
                        }
                    }
                    m = st;
                    break;
                case Answer: {
                    m = new Stimulus.QuestionStimulus.Answer();
                    m.message_type = MessageType.Answer;
                    while (in.hasNext())
                        switch (in.nextName()) {
                            case "lesson_id":
                                ((Stimulus.QuestionStimulus.Answer) m).lesson_id = in.nextLong();
                                break;
                            case "question_id":
                                ((Stimulus.QuestionStimulus.Answer) m).lesson_id = in.nextInt();
                                break;
                            case "answer":
                                ((Stimulus.QuestionStimulus.Answer) m).answer = in.nextInt();
                                break;
                        }
                    break;
                }
                case RemoveStimulus:
                    m = new RemoveStimulus();
                    m.message_type = MessageType.RemoveStimulus;
                    while (in.hasNext())
                        switch (in.nextName()) {
                            case "lesson_id":
                                ((RemoveStimulus) m).lesson_id = in.nextLong();
                                break;
                            case "id":
                                ((RemoveStimulus) m).id = in.nextInt();
                                break;
                        }
                    break;
            }
            in.endObject();
            return m;
        }
    }
}
