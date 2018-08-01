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
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.TimeUnit;
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
public class LessonModel {

    public static final LessonModelAdapter ADAPTER = new LessonModelAdapter();
    public Long id;
    public String name;
    @JsonbTypeAdapter(StimuliTemplateAdapter.class)
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

    public static class LessonModelAdapter implements JsonbAdapter<LessonModel, JsonObject> {

        @Override
        public JsonObject adaptToJson(LessonModel obj) throws Exception {
            JsonObjectBuilder lesson_model_builder = Json.createObjectBuilder();
            if (obj.id != null) {
                lesson_model_builder.add("id", obj.id);
            }
            lesson_model_builder.add("name", obj.name);
            JsonArrayBuilder stimuli_builder = Json.createArrayBuilder();
            for (StimulusTemplate stimulus_template : obj.stimuli.values()) {
                stimuli_builder.add(StimulusTemplate.ADAPTER.adaptToJson(stimulus_template));
            }
            lesson_model_builder.add("stimuli", stimuli_builder);
            JsonArrayBuilder ids_builder = Json.createArrayBuilder();
            for (String id : obj.ids) {
                ids_builder.add(id);
            }
            lesson_model_builder.add("ids", ids_builder);
            JsonArrayBuilder rels_builder = Json.createArrayBuilder();
            for (Relation rel : obj.relations) {
                rels_builder.add(Relation.ADAPTER.adaptToJson(rel));
            }
            lesson_model_builder.add("relations", rels_builder);
            return lesson_model_builder.build();
        }

        @Override
        public LessonModel adaptFromJson(JsonObject obj) throws Exception {
            Map<String, StimulusTemplate> stimuli = new HashMap<>(obj.getJsonArray("stimuli").size());
            for (JsonValue stimulus : obj.getJsonArray("stimuli")) {
                StimulusTemplate template = StimulusTemplate.ADAPTER.adaptFromJson(stimulus.asJsonObject());
                stimuli.put(template.name, template);
            }
            Set<String> ids = new HashSet<>(obj.getJsonArray("ids").size());
            for (JsonValue id : obj.getJsonArray("ids")) {
                ids.add(((JsonString) id).getString());
            }
            List<Relation> relations = new ArrayList<>(obj.getJsonArray("relations").size());
            for (JsonValue rel_val : obj.getJsonArray("relations")) {
                relations.add(Relation.ADAPTER.adaptFromJson(rel_val.asJsonObject()));
            }
            return new LessonModel(obj.containsKey("id") ? new Long(obj.getInt("id")) : null, obj.getString("name"), stimuli, ids, relations);
        }
    }

    public static class ConditionAdapter implements JsonbAdapter<Condition, JsonObject> {

        @Override
        public JsonObject adaptToJson(Condition obj) throws Exception {
            JsonObjectBuilder c_object = Json.createObjectBuilder();
            c_object.add("type", obj.type.name());
            switch (obj.type) {
                case And:
                    JsonArrayBuilder and_condition_builder = Json.createArrayBuilder();
                    for (Condition condition : ((Condition.AndCondition) obj).conditions) {
                        and_condition_builder.add(Condition.ADAPTER.adaptToJson(condition));
                    }
                    c_object.add("conditions", and_condition_builder);
                    break;
                case Or:
                    JsonArrayBuilder or_condition_builder = Json.createArrayBuilder();
                    for (Condition condition : ((Condition.OrCondition) obj).conditions) {
                        or_condition_builder.add(Condition.ADAPTER.adaptToJson(condition));
                    }
                    c_object.add("conditions", or_condition_builder);
                    break;
                case Not:
                    c_object.add("condition", Condition.ADAPTER.adaptToJson(((Condition.NotCondition) obj).condition));
                    break;
                case Numeric:
                    c_object.add("numeric_condition_type", ((Condition.NumericCondition) obj).numeric_condition_type.name());
                    c_object.add("variable", ((Condition.NumericCondition) obj).variable);
                    c_object.add("value", ((Condition.NumericCondition) obj).value);
                    break;
                case Nominal:
                    c_object.add("variable", ((Condition.NominalCondition) obj).variable);
                    c_object.add("value", ((Condition.NominalCondition) obj).value);
                    break;
                default:
                    throw new AssertionError(obj.type.name());
            }
            return c_object.build();
        }

        @Override
        public Condition adaptFromJson(JsonObject obj) throws Exception {
            switch (Condition.ConditionType.valueOf(obj.getString("type"))) {
                case And:
                    JsonArray and_conditions_array = obj.getJsonArray("conditions");
                    List<Condition> and_conditions = new ArrayList<>(and_conditions_array.size());
                    for (JsonValue cond_value : and_conditions_array) {
                        and_conditions.add(Condition.ADAPTER.adaptFromJson(cond_value.asJsonObject()));
                    }
                    return new Condition.AndCondition(and_conditions);
                case Or:
                    JsonArray or_conditions_array = obj.getJsonArray("conditions");
                    List<Condition> or_conditions = new ArrayList<>(or_conditions_array.size());
                    for (JsonValue cond_value : or_conditions_array) {
                        or_conditions.add(Condition.ADAPTER.adaptFromJson(cond_value.asJsonObject()));
                    }
                    return new Condition.OrCondition(or_conditions);
                case Not:
                    return new Condition.NotCondition(Condition.ADAPTER.adaptFromJson(obj.getJsonObject("condition")));
                case Numeric:
                    return new Condition.NumericCondition(Condition.NumericCondition.NumericConditionType.valueOf(obj.getString("numeric_condition_type")), obj.getString("variable"), obj.getJsonNumber("value").doubleValue());
                case Nominal:
                    return new Condition.NominalCondition(obj.getString("variable"), obj.getString("value"));
                default:
                    throw new AssertionError(Condition.ConditionType.valueOf(obj.getString("type")).name());
            }
        }
    }

    public static class StimuliTemplateAdapter implements JsonbAdapter<Map<String, StimulusTemplate>, JsonArray> {

        @Override
        public JsonArray adaptToJson(Map<String, StimulusTemplate> obj) throws Exception {
            JsonArrayBuilder stimuli_builder = Json.createArrayBuilder();
            for (StimulusTemplate st : obj.values()) {
                stimuli_builder.add(StimulusTemplate.ADAPTER.adaptToJson(st));
            }
            return stimuli_builder.build();
        }

        @Override
        public Map<String, StimulusTemplate> adaptFromJson(JsonArray obj) throws Exception {
            Map<String, StimulusTemplate> stimuli = new HashMap<>(obj.size());
            for (JsonValue st : obj) {
                StimulusTemplate template = StimulusTemplate.ADAPTER.adaptFromJson(st.asJsonObject());
                stimuli.put(template.name, template);
            }
            return stimuli;
        }
    }

    public static class StimulusTemplateAdapter implements JsonbAdapter<StimulusTemplate, JsonObject> {

        @Override
        public JsonObject adaptToJson(StimulusTemplate obj) throws Exception {
            JsonObjectBuilder stimulus_builder = Json.createObjectBuilder();
            stimulus_builder.add("type", obj.type.name());
            stimulus_builder.add("name", obj.name);
            JsonArrayBuilder topics_builder = Json.createArrayBuilder();
            for (String topic : obj.topics) {
                topics_builder.add(topic);
            }
            stimulus_builder.add("topics", topics_builder);
            if (obj.trigger_condition != null) {
                stimulus_builder.add("trigger_condition", Condition.ADAPTER.adaptToJson(obj.trigger_condition));
            }
            if (obj.execution_condition != null) {
                stimulus_builder.add("execution_condition", Condition.ADAPTER.adaptToJson(obj.execution_condition));
            }
            JsonArrayBuilder ids_builder = Json.createArrayBuilder();
            for (String id : obj.ids) {
                ids_builder.add(id);
            }
            stimulus_builder.add("ids", ids_builder);
            JsonArrayBuilder rels_builder = Json.createArrayBuilder();
            for (Relation rel : obj.relations) {
                rels_builder.add(Relation.ADAPTER.adaptToJson(rel));
            }
            stimulus_builder.add("relations", rels_builder);
            switch (obj.type) {
                case Root:
                    break;
                case Text:
                    stimulus_builder.add("content", ((StimulusTemplate.TextStimulusTemplate) obj).content);
                    break;
                case URL:
                    stimulus_builder.add("content", ((StimulusTemplate.URLStimulusTemplate) obj).content);
                    stimulus_builder.add("url", ((StimulusTemplate.URLStimulusTemplate) obj).url);
                    break;
                case Question:
                    stimulus_builder.add("question", ((StimulusTemplate.QuestionStimulusTemplate) obj).question);
                    JsonArrayBuilder ans_builder = Json.createArrayBuilder();
                    for (StimulusTemplate.QuestionStimulusTemplate.Answer answer : ((StimulusTemplate.QuestionStimulusTemplate) obj).answers) {
                        ans_builder.add(Json.createObjectBuilder().add("answer", answer.answer).add("event", answer.event));
                    }
                    stimulus_builder.add("answers", ans_builder);
                    break;
                default:
                    throw new AssertionError(obj.type.name());
            }
            return stimulus_builder.build();
        }

        @Override
        public StimulusTemplate adaptFromJson(JsonObject obj) throws Exception {
            String name = obj.getString("name");
            Set<String> topics = new HashSet<>(obj.getJsonArray("topics").size());
            JsonArray topics_array = obj.getJsonArray("topics");
            for (JsonValue topic : topics_array) {
                topics.add(((JsonString) topic).getString());
            }
            Condition trigger_condition = obj.containsKey("trigger_condition") && !obj.isNull("trigger_condition") ? Condition.ADAPTER.adaptFromJson(obj.getJsonObject("trigger_condition")) : null;
            Condition execution_condition = obj.containsKey("execution_condition") && !obj.isNull("execution_condition") ? Condition.ADAPTER.adaptFromJson(obj.getJsonObject("execution_condition")) : null;
            Set<String> ids = new HashSet<>(obj.getJsonArray("ids").size());
            for (JsonValue id : obj.getJsonArray("ids")) {
                ids.add(((JsonString) id).getString());
            }
            List<Relation> relations = new ArrayList<>(obj.getJsonArray("relations").size());
            for (JsonValue rel_val : obj.getJsonArray("relations")) {
                relations.add(Relation.ADAPTER.adaptFromJson(rel_val.asJsonObject()));
            }
            switch (StimulusTemplate.StimulusTemplateType.valueOf(obj.getString("type"))) {
                case Root:
                    return new StimulusTemplate(StimulusTemplate.StimulusTemplateType.Root, name, topics, trigger_condition, execution_condition, ids, relations);
                case Text:
                    return new StimulusTemplate.TextStimulusTemplate(name, topics, trigger_condition, execution_condition, ids, relations, obj.getString("content"));
                case URL:
                    return new StimulusTemplate.URLStimulusTemplate(name, topics, trigger_condition, execution_condition, ids, relations, obj.getString("content"), obj.getString("url"));
                case Question:
                    JsonArray answers_array = obj.getJsonArray("answers");
                    List<StimulusTemplate.QuestionStimulusTemplate.Answer> answers = new ArrayList<>(answers_array.size());
                    for (JsonValue ans_value : answers_array) {
                        JsonObject ans_onject = ans_value.asJsonObject();
                        answers.add(new StimulusTemplate.QuestionStimulusTemplate.Answer(ans_onject.getString("answer"), ans_onject.getString("event")));
                    }
                    return new StimulusTemplate.QuestionStimulusTemplate(name, topics, trigger_condition, execution_condition, ids, relations, obj.getString("question"), answers);
                default:
                    throw new AssertionError(StimulusTemplate.StimulusTemplateType.valueOf(obj.getString("type")).name());
            }
        }
    }

    public static class RelationAdapter implements JsonbAdapter<Relation, JsonObject> {

        @Override
        public JsonObject adaptToJson(Relation obj) throws Exception {
            JsonObjectBuilder rel_builder = Json.createObjectBuilder();
            rel_builder.add("from", obj.from);
            rel_builder.add("to", obj.to);
            if (obj.lb != Double.NEGATIVE_INFINITY) {
                rel_builder.add("lb", obj.lb);
            }
            if (obj.ub != Double.POSITIVE_INFINITY) {
                rel_builder.add("ub", obj.ub);
            }
            rel_builder.add("unit", obj.unit.name());
            return rel_builder.build();
        }

        @Override
        public Relation adaptFromJson(JsonObject obj) throws Exception {
            return new Relation(obj.getString("from"), obj.getString("to"), obj.containsKey("lb") ? obj.getJsonNumber("lb").longValue() : null, obj.containsKey("ub") ? obj.getJsonNumber("ub").longValue() : null, TimeUnit.valueOf(obj.getString("unit")));
        }
    }
}
