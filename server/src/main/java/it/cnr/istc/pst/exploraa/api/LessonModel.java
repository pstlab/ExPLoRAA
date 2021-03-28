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
    private final Map<Long, Stimulus> stimuli;

    @JsonCreator
    public LessonModel(@JsonProperty("id") final long id, @JsonProperty("name") final String name,
            @JsonProperty("id") final Map<Long, Stimulus> stimuli) {
        this.id = id;
        this.name = name;
        this.stimuli = stimuli;
    }

    public long getId() {
        return id;
    }

    public String getName() {
        return name;
    }

    public Map<Long, Stimulus> getStimuli() {
        if (stimuli == null)
            return null;
        return Collections.unmodifiableMap(stimuli);
    }

    @JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property = "type")
    @JsonSubTypes({ @Type(value = Stimulus.WebStimulus.class, name = "web") })
    public static abstract class Stimulus {

        private final long id;
        private final Set<String> topics;
        private final long length;
        private final Set<Long> preconditions;

        @JsonCreator
        public Stimulus(@JsonProperty("id") final long id, @JsonProperty("topics") final Set<String> topics,
                @JsonProperty("length") final long length,
                @JsonProperty("preconditions") final Set<Long> preconditions) {
            this.id = id;
            this.topics = topics;
            this.length = length;
            this.preconditions = preconditions;
        }

        public long getId() {
            return id;
        }

        public Set<String> getTopics() {
            if (topics == null)
                return null;
            return Collections.unmodifiableSet(topics);
        }

        public long getLength() {
            return length;
        }

        public Set<Long> getPreconditions() {
            if (preconditions == null)
                return null;
            return Collections.unmodifiableSet(preconditions);
        }

        public static class WebStimulus extends Stimulus {

            private final String url;

            public WebStimulus(@JsonProperty("id") final long id, @JsonProperty("topics") final Set<String> topics,
                    @JsonProperty("length") final long length,
                    @JsonProperty("preconditions") final Set<Long> preconditions,
                    @JsonProperty("url") final String url) {
                super(id, topics, length, preconditions);
                this.url = url;
            }

            public String getUrl() {
                return url;
            }
        }
    }
}
