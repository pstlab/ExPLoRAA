package it.cnr.istc.exploraa.api.adapters;

import java.io.IOException;
import java.util.HashSet;

import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonToken;
import com.google.gson.stream.JsonWriter;

import it.cnr.istc.exploraa.api.Message;
import it.cnr.istc.exploraa.api.Parameter;
import it.cnr.istc.exploraa.api.User;
import it.cnr.istc.exploraa.api.Message.FollowLesson;
import it.cnr.istc.exploraa.api.Message.MessageType;
import it.cnr.istc.exploraa.api.Message.NewParameter;
import it.cnr.istc.exploraa.api.Message.RemoveLesson;
import it.cnr.istc.exploraa.api.Message.RemoveParameter;
import it.cnr.istc.exploraa.api.Message.RemoveStimulus;
import it.cnr.istc.exploraa.api.Message.RemoveToken;
import it.cnr.istc.exploraa.api.Message.Stimulus;
import it.cnr.istc.exploraa.api.Message.Token;
import it.cnr.istc.exploraa.api.Message.TokenUpdate;
import it.cnr.istc.exploraa.api.Message.UnfollowLesson;

/**
 * MessageAdapter
 */
public class MessageAdapter extends TypeAdapter<Message> {

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
            if (((Token) value).question != null) {
                out.name("question").value(((Token) value).question);
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
                case "question":
                    ((Token) m).question = in.nextInt();
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