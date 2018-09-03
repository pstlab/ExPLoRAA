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
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.TimeUnit;

/**
 * @author Riccardo De Benedictis
 */
public class LessonModel {

    public static final LessonModelAdapter ADAPTER = new LessonModelAdapter();
    public Long id;
    public String name;
    @JsonAdapter(StimuliTemplateAdapter.class)
    public Map<String, StimulusTemplate> stimuli;
    public Set<String> ids;
    public List<Relation> relations;

    public LessonModel() {
    }

    public LessonModel(Long id, String name, Map<String, StimulusTemplate> stimuli, Set<String> ids, List<Relation> relations) {
        this.id = id;
        this.name = name;
        this.stimuli = stimuli;
        this.ids = ids;
        this.relations = relations;
    }

    public static class StimulusTemplate {

        public static final StimulusTemplateAdapter ADAPTER = new StimulusTemplateAdapter();
        public StimulusTemplateType type;
        public String name;
        public Set<String> topics;
        public Condition trigger_condition;
        public Condition execution_condition;
        public Set<String> ids;
        public List<Relation> relations;

        public StimulusTemplate() {
        }

        public StimulusTemplate(StimulusTemplateType type, String name, Set<String> topics, Condition trigger_condition, Condition execution_condition, Set<String> ids, List<Relation> relations) {
            this.type = type;
            this.name = name;
            this.topics = topics;
            this.trigger_condition = trigger_condition;
            this.execution_condition = execution_condition;
            this.ids = ids;
            this.relations = relations;
        }

        public enum StimulusTemplateType {
            Root, Text, URL, Question
        }

        public static class URLStimulusTemplate extends StimulusTemplate {

            public String content;
            public String url;

            public URLStimulusTemplate() {
            }

            public URLStimulusTemplate(String name, Set<String> topics, Condition trigger_condition, Condition execution_condition, Set<String> ids, List<Relation> relations, String content, String url) {
                super(StimulusTemplateType.URL, name, topics, trigger_condition, execution_condition, ids, relations);
                this.content = content;
                this.url = url;
            }
        }

        public static class TextStimulusTemplate extends StimulusTemplate {

            public String content;

            public TextStimulusTemplate() {
            }

            public TextStimulusTemplate(String name, Set<String> topics, Condition trigger_condition, Condition execution_condition, Set<String> ids, List<Relation> relations, String content) {
                super(StimulusTemplateType.Text, name, topics, trigger_condition, execution_condition, ids, relations);
                this.content = content;
            }
        }

        public static class QuestionStimulusTemplate extends StimulusTemplate {

            public String question;
            public List<Answer> answers;

            public QuestionStimulusTemplate() {
            }

            public QuestionStimulusTemplate(String name, Set<String> topics, Condition trigger_condition, Condition execution_condition, Set<String> ids, List<Relation> relations, String question, List<Answer> answers) {
                super(StimulusTemplateType.Question, name, topics, trigger_condition, execution_condition, ids, relations);
                this.question = question;
                this.answers = answers;
            }

            public static class Answer {

                public String answer;
                public String event;

                public Answer() {
                }

                public Answer(String answer, String event) {
                    this.answer = answer;
                    this.event = event;
                }
            }
        }
    }

    public static class Condition {

        public static final ConditionAdapter ADAPTER = new ConditionAdapter();
        public ConditionType type;

        public Condition() {
        }

        public Condition(ConditionType type) {
            this.type = type;
        }

        public enum ConditionType {
            And, Or, Not, Numeric, Nominal
        }

        public static class AndCondition extends Condition {

            public List<Condition> conditions;

            public AndCondition() {
            }

            public AndCondition(Condition... conditions) {
                this(Arrays.asList(conditions));
            }

            public AndCondition(List<Condition> conditions) {
                super(ConditionType.And);
                this.conditions = conditions;
            }
        }

        public static class OrCondition extends Condition {

            public List<Condition> conditions;

            public OrCondition() {
            }

            public OrCondition(Condition... conditions) {
                this(Arrays.asList(conditions));
            }

            public OrCondition(List<Condition> conditions) {
                super(ConditionType.And);
                this.conditions = conditions;
            }
        }

        public static class NotCondition extends Condition {

            public Condition condition;

            public NotCondition() {
            }

            public NotCondition(Condition condition) {
                super(ConditionType.Not);
                this.condition = condition;
            }
        }

        public static class NominalCondition extends Condition {

            public String variable;
            public String value;

            public NominalCondition() {
            }

            public NominalCondition(String variable, String value) {
                super(ConditionType.Nominal);
                this.variable = variable;
                this.value = value;
            }
        }

        public static class NumericCondition extends Condition {

            public NumericConditionType numeric_condition_type;
            public String variable;
            public double value;

            public NumericCondition() {
            }

            public NumericCondition(NumericConditionType type, String variable, double value) {
                super(ConditionType.Numeric);
                this.numeric_condition_type = type;
                this.variable = variable;
                this.value = value;
            }

            public enum NumericConditionType {
                GEq, Eq, LEq
            }
        }
    }

    public static class Relation {

        public static final RelationAdapter ADAPTER = new RelationAdapter();
        public String from;
        public String to;
        public Long lb;
        public Long ub;
        public TimeUnit unit;

        public Relation() {
        }

        public Relation(String from, String to, Long lb, Long ub, TimeUnit unit) {
            this.from = from;
            this.to = to;
            this.lb = lb;
            this.ub = ub;
            this.unit = unit;
        }
    }

    public static class LessonModelAdapter extends TypeAdapter<LessonModel> {

        @Override
        public void write(JsonWriter out, LessonModel value) throws IOException {
            out.beginObject();
            if (value.id != null)
                out.name("id").value(value.id);

            out.name("name").value(value.name);

            out.name("stimuli");
            out.beginArray();
            for (StimulusTemplate stimulus_template : value.stimuli.values())
                StimulusTemplate.ADAPTER.write(out, stimulus_template);
            out.endArray();

            out.name("ids");
            out.beginArray();
            for (String id : value.ids)
                out.value(id);
            out.endArray();

            out.name("relations");
            out.beginArray();
            for (Relation rel : value.relations)
                Relation.ADAPTER.write(out, rel);
            out.endArray();

            out.endObject();
        }

        @Override
        public LessonModel read(JsonReader in) throws IOException {
            LessonModel model = new LessonModel();
            in.beginObject();
            while (in.hasNext())
                switch ((in.nextName())) {
                    case "id":
                        model.id = in.nextLong();
                        break;
                    case "name":
                        model.name = in.nextString();
                        break;
                    case "stimuli":
                        model.stimuli = new HashMap<>();
                        in.beginArray();
                        while (in.peek() != JsonToken.END_ARRAY) {
                            StimulusTemplate st = StimulusTemplate.ADAPTER.read(in);
                            model.stimuli.put(st.name, st);
                        }
                        in.endArray();
                        break;
                    case "ids":
                        model.ids = new HashSet<>();
                        in.beginArray();
                        while (in.peek() != JsonToken.END_ARRAY) model.ids.add(in.nextString());
                        in.endArray();
                        break;
                    case "relations":
                        model.relations = new ArrayList<>();
                        in.beginArray();
                        while (in.peek() != JsonToken.END_ARRAY) {
                            Relation rel = Relation.ADAPTER.read(in);
                            model.relations.add(rel);
                        }
                        in.endArray();
                        break;
                }
            in.endObject();
            return model;
        }
    }

    public static class ConditionAdapter extends TypeAdapter<Condition> {

        @Override
        public void write(JsonWriter out, Condition value) throws IOException {
            out.beginObject();
            out.name("type").value(value.type.name());
            switch (value.type) {
                case And:
                    out.name("conditions");
                    out.beginArray();
                    for (Condition condition : ((Condition.AndCondition) value).conditions)
                        Condition.ADAPTER.write(out, condition);
                    out.endArray();
                    break;
                case Or:
                    out.name("conditions");
                    out.beginArray();
                    for (Condition condition : ((Condition.AndCondition) value).conditions)
                        Condition.ADAPTER.write(out, condition);
                    out.endArray();
                    break;
                case Not:
                    Condition.ADAPTER.write(out.name("conditions"), ((Condition.NotCondition) value).condition);
                    break;
                case Numeric:
                    out.name("numeric_condition_type").value(((Condition.NumericCondition) value).numeric_condition_type.name());
                    out.name("variable").value(((Condition.NumericCondition) value).variable);
                    out.name("value").value(((Condition.NumericCondition) value).value);
                    break;
                case Nominal:
                    out.name("variable").value(((Condition.NominalCondition) value).variable);
                    out.name("value").value(((Condition.NominalCondition) value).value);
                    break;
            }
            out.endObject();
        }

        @Override
        public Condition read(JsonReader in) throws IOException {
            Condition c = null;
            in.beginObject();
            in.nextName();
            switch (Condition.ConditionType.valueOf(in.nextString())) {
                case And:
                    in.nextName();
                    List<Condition> and_conditions = new ArrayList<>();
                    in.beginArray();
                    while (in.peek() != JsonToken.END_ARRAY)
                        and_conditions.add(Condition.ADAPTER.read(in));
                    in.endArray();
                    c = new Condition.AndCondition(and_conditions);
                    break;
                case Or:
                    in.nextName();
                    List<Condition> or_conditions = new ArrayList<>();
                    in.beginArray();
                    while (in.peek() != JsonToken.END_ARRAY)
                        or_conditions.add(Condition.ADAPTER.read(in));
                    in.endArray();
                    c = new Condition.OrCondition(or_conditions);
                    break;
                case Not:
                    in.nextName();
                    c = new Condition.NotCondition(Condition.ADAPTER.read(in));
                    break;
                case Numeric: {
                    Condition.NumericCondition.NumericConditionType n_cond_type = null;
                    String variable = null;
                    double value = -1;
                    while (in.hasNext())
                        switch ((in.nextName())) {
                            case "numeric_condition_type":
                                n_cond_type = Condition.NumericCondition.NumericConditionType.valueOf(in.nextString());
                                break;
                            case "variable":
                                variable = in.nextString();
                                break;
                            case "value":
                                value = in.nextDouble();
                                break;
                        }
                    c = new Condition.NumericCondition(n_cond_type, variable, value);
                    break;
                }
                case Nominal: {
                    String variable = null;
                    String value = null;
                    while (in.hasNext())
                        switch ((in.nextName())) {
                            case "variable":
                                variable = in.nextString();
                                break;
                            case "value":
                                value = in.nextString();
                                break;
                        }
                    c = new Condition.NominalCondition(variable, value);
                    break;
                }
            }
            in.endObject();
            return c;
        }
    }

    public static class StimuliTemplateAdapter extends TypeAdapter<Map<String, StimulusTemplate>> {

        @Override
        public void write(JsonWriter out, Map<String, StimulusTemplate> value) throws IOException {
            out.beginArray();
            for (StimulusTemplate st : value.values())
                StimulusTemplate.ADAPTER.write(out, st);
            out.endArray();
        }

        @Override
        public Map<String, StimulusTemplate> read(JsonReader in) throws IOException {
            Map<String, StimulusTemplate> stimuli = new HashMap<>();
            in.beginArray();
            while (in.peek() != JsonToken.END_ARRAY) {
                StimulusTemplate st = StimulusTemplate.ADAPTER.read(in);
                stimuli.put(st.name, st);
            }
            in.endArray();
            return stimuli;
        }
    }

    public static class StimulusTemplateAdapter extends TypeAdapter<StimulusTemplate> {

        @Override
        public void write(JsonWriter out, StimulusTemplate value) throws IOException {
            out.beginObject();
            out.name("type").value(value.type.name());
            out.name("name").value(value.name);

            out.name("topics");
            out.beginArray();
            for (String topic : value.topics) out.value(topic);
            out.endArray();

            if (value.trigger_condition != null)
                Condition.ADAPTER.write(out.name("trigger_condition"), value.trigger_condition);
            if (value.execution_condition != null)
                Condition.ADAPTER.write(out.name("execution_condition"), value.execution_condition);

            if (value.ids != null) {
                out.name("ids");
                out.beginArray();
                for (String id : value.ids) out.value(id);
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
                        while (in.peek() != JsonToken.END_ARRAY) st.topics.add(in.nextString());
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
                        while (in.peek() != JsonToken.END_ARRAY) st.ids.add(in.nextString());
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
                            case Text:
                                ((StimulusTemplate.TextStimulusTemplate) st).content = in.nextString();
                                break;
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
                            ((StimulusTemplate.QuestionStimulusTemplate) st).answers.add(new StimulusTemplate.QuestionStimulusTemplate.Answer(answer, event));
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

    public static class RelationAdapter extends TypeAdapter<Relation> {

        @Override
        public void write(JsonWriter out, Relation value) throws IOException {
            out.beginObject();
            out.name("from").value(value.from);
            out.name("to").value(value.to);
            if (value.lb != Double.NEGATIVE_INFINITY)
                out.name("lb").value(value.lb);
            if (value.ub != Double.NEGATIVE_INFINITY)
                out.name("ub").value(value.ub);
            out.name("unit").value(value.unit.name());
            out.endObject();
        }

        @Override
        public Relation read(JsonReader in) throws IOException {
            Relation rel = new Relation();
            in.beginObject();
            while (in.hasNext())
                switch ((in.nextName())) {
                    case "from":
                        rel.from = in.nextString();
                        break;
                    case "to":
                        rel.to = in.nextString();
                        break;
                    case "lb":
                        rel.lb = in.nextLong();
                        break;
                    case "ub":
                        rel.ub = in.nextLong();
                        break;
                    case "unit":
                        rel.unit = TimeUnit.valueOf(in.nextString());
                        break;
                }
            in.endObject();
            return rel;
        }
    }
}
