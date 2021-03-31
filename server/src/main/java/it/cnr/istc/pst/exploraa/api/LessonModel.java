package it.cnr.istc.pst.exploraa.api;

import java.util.Collections;
import java.util.Map;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonSubTypes.Type;
import com.fasterxml.jackson.annotation.JsonTypeInfo;

public class LessonModel {

    private final long id;
    private final String name;
    private final Map<Long, Rule> rules;

    @JsonCreator
    public LessonModel(@JsonProperty("id") final long id, @JsonProperty("name") final String name,
            @JsonProperty("rules") final Map<Long, Rule> rules) {
        this.id = id;
        this.name = name;
        this.rules = rules;
    }

    public long getId() {
        return id;
    }

    public String getName() {
        return name;
    }

    public Map<Long, Rule> getRules() {
        if (rules == null)
            return null;
        return Collections.unmodifiableMap(rules);
    }

    @JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property = "type")
    @JsonSubTypes({ @Type(value = Rule.WebRule.class, name = "web") })
    public static abstract class Rule {

        private final long id;
        private final String name;
        private final Set<String> topics;
        private final Long length;
        private final Set<Long> preconditions;

        @JsonCreator
        public Rule(@JsonProperty("id") final long id, @JsonProperty("name") final String name,
                @JsonProperty("topics") final Set<String> topics, @JsonProperty("length") final Long length,
                @JsonProperty("preconditions") final Set<Long> preconditions) {
            this.id = id;
            this.name = name;
            this.topics = topics;
            this.length = length;
            this.preconditions = preconditions;
        }

        public long getId() {
            return id;
        }

        public String getName() {
            return name;
        }

        public Set<String> getTopics() {
            if (topics == null)
                return null;
            return Collections.unmodifiableSet(topics);
        }

        public Long getLength() {
            return length;
        }

        public Set<Long> getPreconditions() {
            if (preconditions == null)
                return null;
            return Collections.unmodifiableSet(preconditions);
        }

        public static class WebRule extends Rule {

            private final String url;

            public WebRule(@JsonProperty("id") final long id, @JsonProperty("name") final String name,
                    @JsonProperty("topics") final Set<String> topics, @JsonProperty("length") final Long length,
                    @JsonProperty("preconditions") final Set<Long> preconditions,
                    @JsonProperty("url") final String url) {
                super(id, name, topics, length, preconditions);
                this.url = url;
            }

            public String getUrl() {
                return url;
            }
        }
    }
}
