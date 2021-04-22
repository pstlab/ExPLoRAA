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
    @JsonSubTypes({ @Type(value = Rule.TextRule.class, name = "text"), @Type(value = Rule.WebRule.class, name = "web"),
            @Type(value = Rule.WikiRule.class, name = "wiki") })
    public static abstract class Rule {

        private final long id;
        private final String name;
        private final Set<String> topics;
        private final Long length;
        private final Set<Long> preconditions;
        private final Set<String> suggestions;

        @JsonCreator
        public Rule(@JsonProperty("id") final long id, @JsonProperty("name") final String name,
                @JsonProperty("topics") final Set<String> topics, @JsonProperty("length") final Long length,
                @JsonProperty("preconditions") final Set<Long> preconditions,
                @JsonProperty("suggestions") final Set<String> suggestions) {
            this.id = id;
            this.name = name;
            this.topics = topics;
            this.length = length;
            this.preconditions = preconditions;
            this.suggestions = suggestions;
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

        public Set<String> getSuggestions() {
            if (suggestions == null)
                return null;
            return Collections.unmodifiableSet(suggestions);
        }

        public static class TextRule extends Rule {

            private final String text;

            public TextRule(@JsonProperty("id") final long id, @JsonProperty("name") final String name,
                    @JsonProperty("topics") final Set<String> topics, @JsonProperty("length") final Long length,
                    @JsonProperty("preconditions") final Set<Long> preconditions,
                    @JsonProperty("suggestions") final Set<String> suggestions,
                    @JsonProperty("text") final String text) {
                super(id, name, topics, length, preconditions, suggestions);
                this.text = text;
            }

            public String getText() {
                return text;
            }
        }

        public static class WebRule extends Rule {

            private final String url;

            public WebRule(@JsonProperty("id") final long id, @JsonProperty("name") final String name,
                    @JsonProperty("topics") final Set<String> topics, @JsonProperty("length") final Long length,
                    @JsonProperty("preconditions") final Set<Long> preconditions,
                    @JsonProperty("suggestions") final Set<String> suggestions, @JsonProperty("url") final String url) {
                super(id, name, topics, length, preconditions, suggestions);
                this.url = url;
            }

            public String getUrl() {
                return url;
            }
        }

        public static class WikiRule extends Rule {

            private final String url;

            public WikiRule(@JsonProperty("id") final long id, @JsonProperty("name") final String name,
                    @JsonProperty("topics") final Set<String> topics, @JsonProperty("length") final Long length,
                    @JsonProperty("preconditions") final Set<Long> preconditions,
                    @JsonProperty("suggestions") final Set<String> suggestions, @JsonProperty("url") final String url) {
                super(id, name, topics, length, preconditions, suggestions);
                this.url = url;
            }

            public String getUrl() {
                return url;
            }
        }
    }
}
