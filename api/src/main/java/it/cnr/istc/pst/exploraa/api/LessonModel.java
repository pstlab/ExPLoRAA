package it.cnr.istc.pst.exploraa.api;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.TimeUnit;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.annotation.JsonSubTypes.Type;

/**
 * LessonModel
 */
public class LessonModel {

    private final Long id;
    private final String name;
    private final Map<String, StimulusTemplate> stimuli;
    private final Set<String> ids;
    private final List<Relation> relations;

    @JsonCreator
    public LessonModel(@JsonProperty("id") Long id, @JsonProperty("name") String name,
            @JsonProperty("stimuli") Map<String, StimulusTemplate> stimuli, @JsonProperty("ids") Set<String> ids,
            @JsonProperty("relations") List<Relation> relations) {
        this.id = id;
        this.name = name;
        this.stimuli = stimuli;
        this.ids = ids;
        this.relations = relations;
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
     * @return the stimuli
     */
    public Map<String, StimulusTemplate> getStimuli() {
        if (stimuli == null)
            return null;
        return Collections.unmodifiableMap(stimuli);
    }

    /**
     * @return the ids
     */
    public Set<String> getIds() {
        if (ids == null)
            return null;
        return Collections.unmodifiableSet(ids);
    }

    /**
     * @return the relations
     */
    public List<Relation> getRelations() {
        if (relations == null)
            return relations;
        return Collections.unmodifiableList(relations);
    }

    @JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property = "type")
    @JsonSubTypes({ @Type(value = URLStimulusTemplate.class, name = "url"),
            @Type(value = TextStimulusTemplate.class, name = "text") })
    public static class StimulusTemplate {

        private final String name;
        private final Set<String> ids;
        private final List<Relation> relations;

        @JsonCreator
        public StimulusTemplate(@JsonProperty("name") String name, @JsonProperty("ids") Set<String> ids,
                @JsonProperty("relations") List<Relation> relations) {
            this.name = name;
            this.ids = ids;
            this.relations = relations;
        }

        /**
         * @return the name
         */
        public String getName() {
            return name;
        }

        /**
         * @return the ids
         */
        public Set<String> getIds() {
            if (ids == null)
                return ids;
            return Collections.unmodifiableSet(ids);
        }

        /**
         * @return the relations
         */
        public List<Relation> getRelations() {
            if (relations == null)
                return relations;
            return Collections.unmodifiableList(relations);
        }
    }

    public static class URLStimulusTemplate extends StimulusTemplate {

        private final Set<String> topics;
        private final String content;
        private final String url;

        @JsonCreator
        public URLStimulusTemplate(@JsonProperty("name") String name, @JsonProperty("ids") Set<String> ids,
                @JsonProperty("relations") List<Relation> relations, @JsonProperty("topics") Set<String> topics,
                @JsonProperty("content") String content, @JsonProperty("url") String url) {
            super(name, ids, relations);
            this.topics = topics;
            this.content = content;
            this.url = url;
        }

        /**
         * @return the topics
         */
        public Set<String> getTopics() {
            if (topics == null)
                return topics;
            return Collections.unmodifiableSet(topics);
        }

        /**
         * @return the content
         */
        public String getContent() {
            return content;
        }

        /**
         * @return the url
         */
        public String getUrl() {
            return url;
        }
    }

    public static class TextStimulusTemplate extends StimulusTemplate {

        private final Set<String> topics;
        private final String content;

        @JsonCreator
        public TextStimulusTemplate(@JsonProperty("name") String name, @JsonProperty("ids") Set<String> ids,
                @JsonProperty("relations") List<Relation> relations, @JsonProperty("topics") Set<String> topics,
                @JsonProperty("content") String content) {
            super(name, ids, relations);
            this.topics = topics;
            this.content = content;
        }

        /**
         * @return the topics
         */
        public Set<String> getTopics() {
            if (topics == null)
                return topics;
            return Collections.unmodifiableSet(topics);
        }

        /**
         * @return the content
         */
        public String getContent() {
            return content;
        }
    }

    public static class Relation {

        private final String from;
        private final String to;
        private final Long lb;
        private final Long ub;
        private final TimeUnit unit;

        @JsonCreator
        public Relation(@JsonProperty("from") String from, @JsonProperty("to") String to, @JsonProperty("lb") Long lb,
                @JsonProperty("ub") Long ub, @JsonProperty("unit") TimeUnit unit) {
            this.from = from;
            this.to = to;
            this.lb = lb;
            this.ub = ub;
            this.unit = unit;
        }

        /**
         * @return the from
         */
        public String getFrom() {
            return from;
        }

        /**
         * @return the to
         */
        public String getTo() {
            return to;
        }

        /**
         * @return the lb
         */
        public Long getLb() {
            return lb;
        }

        /**
         * @return the ub
         */
        public Long getUb() {
            return ub;
        }

        /**
         * @return the unit
         */
        public TimeUnit getUnit() {
            return unit;
        }
    }
}