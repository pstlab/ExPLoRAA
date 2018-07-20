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
import java.util.Collection;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.TimeUnit;

/**
 *
 * @author Riccardo De Benedictis
 */
public class LessonModel {

    public Long id;
    public String name;
    public Map<String, StimulusTemplate> stimuli;
    public Set<String> ids;
    public Collection<Relation> relations;

    public static class StimulusTemplate {

        public StimulusTemplateType type;
        public String name;
        public Set<String> topics;
        public Condition trigger_condition;
        public Condition execution_condition;
        public Set<String> ids;
        public Collection<Relation> relations;

        public StimulusTemplate() {
        }

        public StimulusTemplate(StimulusTemplateType type, String name, Set<String> topics, Condition trigger_condition, Condition execution_condition, Set<String> ids, Collection<Relation> relations) {
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

            public URLStimulusTemplate(String name, Set<String> topics, Condition trigger_condition, Condition execution_condition, Set<String> ids, Collection<Relation> relations, String content, String url) {
                super(StimulusTemplateType.URL, name, topics, trigger_condition, execution_condition, ids, relations);
                this.content = content;
                this.url = url;
            }
        }

        public static class TextStimulusTemplate extends StimulusTemplate {

            public String content;

            public TextStimulusTemplate() {
            }

            public TextStimulusTemplate(String name, Set<String> topics, Condition trigger_condition, Condition execution_condition, Set<String> ids, Collection<Relation> relations, String content) {
                super(StimulusTemplateType.Text, name, topics, trigger_condition, execution_condition, ids, relations);
                this.content = content;
            }
        }

        public static class QuestionStimulusTemplate extends StimulusTemplate {

            public String question;
            public ArrayList<Answer> answers;

            public QuestionStimulusTemplate() {
            }

            public QuestionStimulusTemplate(String name, Set<String> topics, Condition trigger_condition, Condition execution_condition, Set<String> ids, Collection<Relation> relations, String question, Collection<Answer> answers) {
                super(StimulusTemplateType.Question, name, topics, trigger_condition, execution_condition, ids, relations);
                this.question = question;
                this.answers = new ArrayList<>(answers);
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

            public Collection<Condition> conditions;

            public AndCondition() {
            }

            public AndCondition(Condition... conditions) {
                this(Arrays.asList(conditions));
            }

            public AndCondition(Collection<Condition> conditions) {
                super(ConditionType.And);
                this.conditions = conditions;
            }
        }

        public static class OrCondition extends Condition {

            public Collection<Condition> conditions;

            public OrCondition() {
            }

            public OrCondition(Condition... conditions) {
                this(Arrays.asList(conditions));
            }

            public OrCondition(Collection<Condition> conditions) {
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
