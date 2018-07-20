package it.cnr.istc.exploraa;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import it.cnr.istc.exploraa.api.Lesson;
import it.cnr.istc.exploraa.api.Message;

public class FollowingLessonContext {

    private final Lesson lesson;
    private final List<Message.Stimulus> stimuli = new ArrayList<>();
    private final List<FollowingLessonListener> listeners = new ArrayList<>();
    private Lesson.LessonState state = Lesson.LessonState.Stopped;
    private long time = 0;

    FollowingLessonContext(Lesson lesson) {
        this.lesson = lesson;
    }

    public Lesson getLesson() {
        return lesson;
    }

    public Lesson.LessonState getState() {
        return state;
    }

    public void setState(Lesson.LessonState state) {
        if (this.state != state) {
            this.state = state;
            for (FollowingLessonListener l : listeners) l.stateChanged(state);
        }
    }

    public long getTime() {
        return time;
    }

    public void setTime(long time) {
        if (this.time != time) {
            this.time = time;
            for (FollowingLessonListener l : listeners) l.timeChanged(time);
        }
    }

    public List<Message.Stimulus> getStimuli() {
        return Collections.unmodifiableList(stimuli);
    }

    public void addStimulus(final Message.Stimulus e) {
        stimuli.add(e);
        for (FollowingLessonListener l : listeners) l.addedStimulus(e);
    }

    public void removeStimulus(final Message.Stimulus e) {
        stimuli.remove(e);
        for (FollowingLessonListener l : listeners) l.removedStimulus(e);
    }

    public void addListener(FollowingLessonListener l) {
        listeners.add(l);
    }

    public void removeListener(FollowingLessonListener l) {
        listeners.remove(l);
    }

    public interface FollowingLessonListener {

        void timeChanged(long t);

        void stateChanged(Lesson.LessonState state);

        void addedStimulus(Message.Stimulus e);

        void removedStimulus(Message.Stimulus e);
    }
}
