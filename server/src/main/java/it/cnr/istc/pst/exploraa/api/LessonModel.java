package it.cnr.istc.pst.exploraa.api;

import java.util.Collections;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonSubTypes.Type;
import com.fasterxml.jackson.annotation.JsonTypeInfo;

public class LessonModel {

    private final Long id;
    private final String name;
    private final Map<String, Rule> rules;
    private final Requirement requirement;

    @JsonCreator
    public LessonModel(@JsonProperty("id") Long id, @JsonProperty("name") String name,
            @JsonProperty("rules") Map<String, Rule> rules, @JsonProperty("requirement") Requirement requirement) {
        this.id = id;
        this.name = name;
        this.rules = rules;
        this.requirement = requirement;
    }

    /**
     * @return the id
     */
    public Long getId() {
        return id;
    }

    /**
     * @return the name
     */
    public String getName() {
        return name;
    }

    /**
     * @return the rules
     */
    public Map<String, Rule> getRules() {
        return Collections.unmodifiableMap(rules);
    }

    /**
     * @return the requirement
     */
    public Requirement getRequirement() {
        return requirement;
    }

    public static class Rule extends Requirement {

        private final String name;
        private final Requirement requirement;

        @JsonCreator
        public Rule(@JsonProperty("name") String name, @JsonProperty("requirement") Requirement requirement) {
            this.name = name;
            this.requirement = requirement;
        }

        /**
         * @return the name
         */
        public String getName() {
            return name;
        }

        /**
         * @return the requirement
         */
        public Requirement getRequirement() {
            return requirement;
        }
    }

    @JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property = "type")
    @JsonSubTypes({ @Type(value = Requirement.And.class, name = "and"),
            @Type(value = Requirement.Or.class, name = "or"), @Type(value = Requirement.Token.class, name = "token"),
            @Type(value = Requirement.Constraint.class, name = "constraint") })
    public static abstract class Requirement {

        public static class And extends Requirement {

            private final List<Requirement> requirements;

            @JsonCreator
            public And(@JsonProperty("requirements") List<Requirement> requirements) {
                this.requirements = requirements;
            }

            public List<Requirement> getRequirements() {
                return Collections.unmodifiableList(requirements);
            }
        }

        public static class Or extends Requirement {

            private final List<Requirement> requirements;

            @JsonCreator
            public Or(@JsonProperty("requirements") List<Requirement> requirements) {
                this.requirements = requirements;
            }

            public List<Requirement> getRequirements() {
                return Collections.unmodifiableList(requirements);
            }
        }

        public static class Token extends Requirement {

            private final String name;

            @JsonCreator
            public Token(@JsonProperty("name") String name) {
                this.name = name;
            }

            /**
             * @return the name
             */
            public String getName() {
                return name;
            }
        }

        public static class Constraint extends Requirement {

            private final String constraint;

            @JsonCreator
            public Constraint(@JsonProperty("constraint") String constraint) {
                this.constraint = constraint;
            }

            /**
             * @return the constraint
             */
            public String getConstraint() {
                return constraint;
            }
        }
    }
}
