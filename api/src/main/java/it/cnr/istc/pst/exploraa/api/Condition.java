package it.cnr.istc.pst.exploraa.api;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.annotation.JsonSubTypes.Type;

/**
 * Condition
 */
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property = "type")
@JsonSubTypes({ @Type(value = Condition.AndCondition.class, name = "and"),
        @Type(value = Condition.OrCondition.class, name = "or"),
        @Type(value = Condition.NotCondition.class, name = "not"),
        @Type(value = Condition.NominalCondition.class, name = "nominal"),
        @Type(value = Condition.NumericCondition.class, name = "numeric") })
public class Condition {

    public static class AndCondition extends Condition {

        private final List<Condition> conditions;

        public AndCondition(Condition... conditions) {
            this(Arrays.asList(conditions));
        }

        @JsonCreator
        public AndCondition(@JsonProperty("conditions") List<Condition> conditions) {
            this.conditions = conditions;
        }

        /**
         * @return the conditions
         */
        public List<Condition> getConditions() {
            return Collections.unmodifiableList(conditions);
        }
    }

    public static class OrCondition extends Condition {

        private final List<Condition> conditions;

        public OrCondition(Condition... conditions) {
            this(Arrays.asList(conditions));
        }

        @JsonCreator
        public OrCondition(@JsonProperty("conditions") List<Condition> conditions) {
            this.conditions = conditions;
        }

        /**
         * @return the conditions
         */
        public List<Condition> getConditions() {
            return Collections.unmodifiableList(conditions);
        }
    }

    public static class NotCondition extends Condition {

        private final Condition condition;

        @JsonCreator
        public NotCondition(@JsonProperty("condition") Condition condition) {
            this.condition = condition;
        }

        /**
         * @return the condition
         */
        public Condition getCondition() {
            return condition;
        }
    }

    public static class NominalCondition extends Condition {

        private final String variable;
        private final String value;

        @JsonCreator
        public NominalCondition(@JsonProperty("variable") String variable, @JsonProperty("value") String value) {
            this.variable = variable;
            this.value = value;
        }

        /**
         * @return the variable
         */
        public String getVariable() {
            return variable;
        }

        /**
         * @return the value
         */
        public String getValue() {
            return value;
        }
    }

    public static class NumericCondition extends Condition {

        private final NumericConditionType type;
        private final String variable;
        private final double value;

        @JsonCreator
        public NumericCondition(@JsonProperty("numeric-condition-type") NumericConditionType type,
                @JsonProperty("variable") String variable, @JsonProperty("value") double value) {
            this.type = type;
            this.variable = variable;
            this.value = value;
        }

        /**
         * @return the type
         */
        public NumericConditionType getType() {
            return type;
        }

        /**
         * @return the variable
         */
        public String getVariable() {
            return variable;
        }

        /**
         * @return the value
         */
        public double getValue() {
            return value;
        }

        public enum NumericConditionType {
            GEq, Eq, LEq
        }
    }
}