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

    FollowingLessonContext(Lesson lesson) {
        this.lesson = lesson;
        for (Message.Stimulus stimulus : lesson.stimuli)
            addStimulus(stimulus);
    }

    public Lesson getLesson() {
        return lesson;
    }

    public Lesson.LessonState getState() {
        return lesson.state;
    }

    public void setState(Lesson.LessonState state) {
        if (lesson.state != state) {
            lesson.state = state;
            for (FollowingLessonListener l : listeners) l.stateChanged(state);
        }
    }

    public long getTime() {
        return lesson.time;
    }

    public void setTime(long time) {
        if (lesson.time != time) {
            lesson.time = time;
            for (FollowingLessonListener l : listeners) l.timeChanged(time);
        }
    }

    public List<Message.Stimulus> getStimuli() {
        return Collections.unmodifiableList(stimuli);
    }

    public void addStimulus(final Message.Stimulus stimulus) {
        int pos = stimuli.size();
        stimuli.add(stimulus);
        ExPLoRAAContext.getInstance().addStimulus(stimulus);
        for (FollowingLessonListener l : listeners) l.addedStimulus(pos, stimulus);
    }

    public void removeStimulus(final Message.Stimulus stimulus) {
        int pos = stimuli.indexOf(stimulus);
        stimuli.remove(pos);
        ExPLoRAAContext.getInstance().removeStimulus(stimulus);
        for (FollowingLessonListener l : listeners) l.removedStimulus(pos, stimulus);
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

        void addedStimulus(int position, Message.Stimulus e);

        void removedStimulus(int position, Message.Stimulus e);
    }
}
