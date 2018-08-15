package it.cnr.istc.exploraa.api;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.TimeUnit;

import com.google.gson.annotations.JsonAdapter;

import it.cnr.istc.exploraa.api.adapters.ConditionAdapter;
import it.cnr.istc.exploraa.api.adapters.LessonModelAdapter;
import it.cnr.istc.exploraa.api.adapters.RelationAdapter;
import it.cnr.istc.exploraa.api.adapters.StimuliTemplateAdapter;
import it.cnr.istc.exploraa.api.adapters.StimulusTemplateAdapter;

/**
 * LessonModel
 */
@JsonAdapter(LessonModelAdapter.class)
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

    public LessonModel(Long id, String name, Map<String, StimulusTemplate> stimuli, Set<String> ids,
            List<Relation> relations) {
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

        public StimulusTemplate(StimulusTemplateType type, String name, Set<String> topics, Condition trigger_condition,
                Condition execution_condition, Set<String> ids, List<Relation> relations) {
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

            public URLStimulusTemplate(String name, Set<String> topics, Condition trigger_condition,
                    Condition execution_condition, Set<String> ids, List<Relation> relations, String content,
                    String url) {
                super(StimulusTemplateType.URL, name, topics, trigger_condition, execution_condition, ids, relations);
                this.content = content;
                this.url = url;
            }
        }

        public static class TextStimulusTemplate extends StimulusTemplate {

            public String content;

            public TextStimulusTemplate() {
            }

            public TextStimulusTemplate(String name, Set<String> topics, Condition trigger_condition,
                    Condition execution_condition, Set<String> ids, List<Relation> relations, String content) {
                super(StimulusTemplateType.Text, name, topics, trigger_condition, execution_condition, ids, relations);
                this.content = content;
            }
        }

        public static class QuestionStimulusTemplate extends StimulusTemplate {

            public String question;
            public List<Answer> answers;

            public QuestionStimulusTemplate() {
            }

            public QuestionStimulusTemplate(String name, Set<String> topics, Condition trigger_condition,
                    Condition execution_condition, Set<String> ids, List<Relation> relations, String question,
                    List<Answer> answers) {
                super(StimulusTemplateType.Question, name, topics, trigger_condition, execution_condition, ids,
                        relations);
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
}