package it.cnr.istc.pst.exploraa.api;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import it.cnr.istc.pst.exploraa.api.Message.Stimulus;
import it.cnr.istc.pst.exploraa.api.Message.Token;

import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.Set;

/**
 * Lesson
 */
public class Lesson {

    private final long id;
    private final String name;
    private final long model_id;
    private final Set<String> topics;
    private final Teaching teacher;
    private final Map<Long, Following> students;
    private final Collection<Stimulus> stimuli;
    private final Collection<Token> tokens;
    private LessonState state;
    private long time;

    @JsonCreator
    public Lesson(@JsonProperty("id") long id, @JsonProperty("name") String name,
            @JsonProperty("modelId") long model_id, @JsonProperty("topics") Set<String> topics,
            @JsonProperty("teacher") Teaching teacher, @JsonProperty("students") Map<Long, Following> students,
            @JsonProperty("stimuli") Collection<Message.Stimulus> stimuli,
            @JsonProperty("tokens") Collection<Message.Token> tokens, @JsonProperty("state") LessonState state,
            @JsonProperty("time") long time) {
        this.id = id;
        this.name = name;
        this.model_id = model_id;
        this.topics = topics;
        this.teacher = teacher;
        this.students = students;
        this.stimuli = stimuli;
        this.tokens = tokens;
        this.state = state;
        this.time = time;
    }

    /**
     * @return the id
     */
    public long getId() {
        return id;
    }

    /**
     * @return the name
     */
    public String getName() {
        return name;
    }

    /**
     * @return the model id
     */
    public long getModelId() {
        return model_id;
    }

    /**
     * @return the topics
     */
    public Set<String> getTopics() {
        if (topics == null)
            return null;
        return Collections.unmodifiableSet(topics);
    }

    /**
     * @return the teacher
     */
    public Teaching getTeacher() {
        return teacher;
    }

    /**
     * @return the students
     */
    public Map<Long, Following> getStudents() {
        if (students == null)
            return null;
        return Collections.unmodifiableMap(students);
    }

    /**
     * @return the stimuli
     */
    public Collection<Message.Stimulus> getStimuli() {
        if (stimuli == null)
            return null;
        return Collections.unmodifiableCollection(stimuli);
    }

    /**
     * @return the tokens
     */
    public Collection<Message.Token> getTokens() {
        if (tokens == null)
            return null;
        return Collections.unmodifiableCollection(tokens);
    }

    /**
     * @return the state
     */
    public LessonState getState() {
        return state;
    }

    /**
     * @param state the state to set
     */
    public void setState(LessonState state) {
        this.state = state;
    }

    /**
     * @return the time
     */
    public long getTime() {
        return time;
    }

    /**
     * @param time the time to set
     */
    public void setTime(long time) {
        this.time = time;
    }

    public enum LessonState {
        Running, Paused, Stopped
    }
}