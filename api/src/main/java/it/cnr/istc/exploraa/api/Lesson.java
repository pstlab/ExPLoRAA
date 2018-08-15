package it.cnr.istc.exploraa.api;

import java.util.List;
import java.util.Map;
import java.util.Set;

import com.google.gson.annotations.JsonAdapter;

import it.cnr.istc.exploraa.api.adapters.LessonAdapter;
import it.cnr.istc.exploraa.api.adapters.LessonModelAdapter;
import it.cnr.istc.exploraa.api.adapters.StimulusListAdapter;

/**
 * This class represents the ExPLoRAA lesson.
 */
@JsonAdapter(LessonAdapter.class)
public class Lesson {

    public static final LessonAdapter ADAPTER = new LessonAdapter();
    public long id;
    public String name;
    @JsonAdapter(LessonModelAdapter.class)
    public LessonModel model;
    public Set<String> topics;
    @JsonAdapter(StimulusListAdapter.class)
    public List<Message.Stimulus> stimuli;
    public List<Message.Token> tokens;
    public Teach teacher;
    public Map<Long, Follow> students;
    public LessonState state;
    public long time;

    public Lesson() {
    }

    public Lesson(long id, String name, LessonModel model, Set<String> topics, List<Message.Stimulus> stimuli,
            List<Message.Token> tokens, Teach teacher, Map<Long, Follow> students, LessonState state, long time) {
        this.id = id;
        this.name = name;
        this.model = model;
        this.topics = topics;
        this.stimuli = stimuli;
        this.tokens = tokens;
        this.teacher = teacher;
        this.students = students;
        this.state = state;
        this.time = time;
    }

    public enum LessonState {
        Running, Paused, Stopped
    }
}