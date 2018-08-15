package it.cnr.istc.exploraa.api.adapters;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Objects;

import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonToken;
import com.google.gson.stream.JsonWriter;

import it.cnr.istc.exploraa.api.LessonModel.Condition;
import it.cnr.istc.exploraa.api.LessonModel.Relation;
import it.cnr.istc.exploraa.api.LessonModel.StimulusTemplate;

/**
 * StimulusTemplateAdapter
 */
public class StimulusTemplateAdapter extends TypeAdapter<StimulusTemplate> {

    @Override
    public void write(JsonWriter out, StimulusTemplate value) throws IOException {
        out.beginObject();
        out.name("type").value(value.type.name());
        out.name("name").value(value.name);

        out.name("topics");
        out.beginArray();
        for (String topic : value.topics)
            out.value(topic);
        out.endArray();

        if (value.trigger_condition != null)
            Condition.ADAPTER.write(out.name("trigger_condition"), value.trigger_condition);
        if (value.execution_condition != null)
            Condition.ADAPTER.write(out.name("execution_condition"), value.execution_condition);

        if (value.ids != null) {
            out.name("ids");
            out.beginArray();
            for (String id : value.ids)
                out.value(id);
            out.endArray();
        }

        if (value.relations != null) {
            out.name("relations");
            out.beginArray();
            for (Relation rel : value.relations)
                Relation.ADAPTER.write(out, rel);
            out.endArray();
        }

        switch (value.type) {
        case Root:
            break;
        case Text:
            out.name("content").value(((StimulusTemplate.TextStimulusTemplate) value).content);
            break;
        case URL:
            out.name("content").value(((StimulusTemplate.URLStimulusTemplate) value).content);
            out.name("url").value(((StimulusTemplate.URLStimulusTemplate) value).url);
            break;
        case Question:
            out.name("question").value(((StimulusTemplate.QuestionStimulusTemplate) value).question);
            out.name("answers");
            out.beginArray();
            for (StimulusTemplate.QuestionStimulusTemplate.Answer answer : ((StimulusTemplate.QuestionStimulusTemplate) value).answers)
                out.name("answer").value(answer.answer).name("event").value(answer.event);
            out.endArray();
            break;
        }
        out.endObject();
    }

    @Override
    public StimulusTemplate read(JsonReader in) throws IOException {
        StimulusTemplate st = null;
        in.beginObject();
        while (in.hasNext())
            switch ((in.nextName())) {
            case "name":
                Objects.requireNonNull(st).name = in.nextString();
                break;
            case "topics":
                Objects.requireNonNull(st).topics = new HashSet<>();
                in.beginArray();
                while (in.peek() != JsonToken.END_ARRAY)
                    st.topics.add(in.nextString());
                in.endArray();
                break;
            case "trigger_condition":
                Objects.requireNonNull(st).trigger_condition = Condition.ADAPTER.read(in);
                break;
            case "execution_condition":
                Objects.requireNonNull(st).execution_condition = Condition.ADAPTER.read(in);
                break;
            case "ids":
                Objects.requireNonNull(st).ids = new HashSet<>();
                in.beginArray();
                while (in.peek() != JsonToken.END_ARRAY)
                    st.ids.add(in.nextString());
                in.endArray();
                break;
            case "relations":
                Objects.requireNonNull(st).relations = new ArrayList<>();
                in.beginArray();
                while (in.peek() != JsonToken.END_ARRAY)
                    st.relations.add(Relation.ADAPTER.read(in));
                in.endArray();
                break;
            case "content":
                switch (Objects.requireNonNull(st).type) {
                case Root:
                    break;
                case Text:
                    ((StimulusTemplate.TextStimulusTemplate) st).content = in.nextString();
                    break;
                case Question:
                    throw new AssertionError();
                case URL:
                    ((StimulusTemplate.URLStimulusTemplate) st).content = in.nextString();
                    break;
                }
                break;
            case "url":
                ((StimulusTemplate.URLStimulusTemplate) Objects.requireNonNull(st)).url = in.nextString();
                break;
            case "question":
                ((StimulusTemplate.QuestionStimulusTemplate) Objects.requireNonNull(st)).question = in.nextString();
                break;
            case "answers":
                ((StimulusTemplate.QuestionStimulusTemplate) Objects.requireNonNull(st)).answers = new ArrayList<>();
                in.beginArray();
                while (in.peek() != JsonToken.END_ARRAY) {
                    String answer = null;
                    String event = null;
                    in.beginObject();
                    for (int i = 0; i < 2; i++) {
                        switch ((in.nextName())) {
                        case "answer":
                            answer = in.nextString();
                            break;
                        case "event":
                            event = in.nextString();
                            break;
                        }
                    }
                    ((StimulusTemplate.QuestionStimulusTemplate) st).answers
                            .add(new StimulusTemplate.QuestionStimulusTemplate.Answer(answer, event));
                    in.endObject();
                }
                in.endArray();
                break;
            case "type":
                switch (StimulusTemplate.StimulusTemplateType.valueOf(in.nextString())) {
                case Root:
                    st = new StimulusTemplate();
                    st.type = StimulusTemplate.StimulusTemplateType.Root;
                    break;
                case Text:
                    st = new StimulusTemplate.TextStimulusTemplate();
                    st.type = StimulusTemplate.StimulusTemplateType.Text;
                    break;
                case URL:
                    st = new StimulusTemplate.URLStimulusTemplate();
                    st.type = StimulusTemplate.StimulusTemplateType.URL;
                    break;
                case Question:
                    st = new StimulusTemplate.QuestionStimulusTemplate();
                    st.type = StimulusTemplate.StimulusTemplateType.Question;
                    break;
                }
                break;
            }
        in.endObject();
        return st;
    }
}