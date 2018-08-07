package it.cnr.istc.exploraa;

import android.content.Intent;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import it.cnr.istc.exploraa.api.Lesson;
import it.cnr.istc.exploraa.api.Message;

public class FollowingLessonContext {

    public static final String FOLLOWING_LESSON_STATE_CHANGED = "Following lesson state changed";
    public static final String FOLLOWING_LESSON_TIME_CHANGED = "Following lesson time changed";
    public static final String UPDATED_FOLLOWING_LESSON = "Following lesson updated";
    public static final String ADDED_LESSON_STIMULUS = "Added lesson stimulus";
    public static final String REMOVED_LESSON_STIMULUS = "Removed lesson stimulus";
    private final ExPLoRAAService service;
    private final Lesson lesson;
    private final List<Message.Stimulus> stimuli = new ArrayList<>();

    FollowingLessonContext(ExPLoRAAService service, Lesson lesson) {
        this.service = service;
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
            Intent lesson_state_changed_intent = new Intent(FOLLOWING_LESSON_STATE_CHANGED + lesson.id);
            lesson_state_changed_intent.putExtra("state", state.name());
            service.sendBroadcast(lesson_state_changed_intent);
            Intent lesson_updated_intent = new Intent(UPDATED_FOLLOWING_LESSON);
            lesson_updated_intent.putExtra("lesson", lesson.id);
            service.sendBroadcast(lesson_updated_intent);
        }
    }

    public long getTime() {
        return lesson.time;
    }

    public void setTime(long time) {
        if (lesson.time != time) {
            lesson.time = time;
            Intent lesson_time_changed_intent = new Intent(FOLLOWING_LESSON_TIME_CHANGED + lesson.id);
            lesson_time_changed_intent.putExtra("time", time);
            service.sendBroadcast(lesson_time_changed_intent);
            Intent lesson_updated_intent = new Intent(UPDATED_FOLLOWING_LESSON);
            lesson_updated_intent.putExtra("lesson", lesson.id);
            service.sendBroadcast(lesson_updated_intent);
        }
    }

    public List<Message.Stimulus> getStimuli() {
        return Collections.unmodifiableList(stimuli);
    }

    public void addStimulus(final Message.Stimulus stimulus) {
        int pos = stimuli.size();
        stimuli.add(stimulus);
        // we add the stimulus to all the received stimuli..
        service.addStimulus(stimulus);
        Intent added_stimulus_intent = new Intent(ADDED_LESSON_STIMULUS + lesson.id);
        added_stimulus_intent.putExtra("position", pos);
        service.sendBroadcast(added_stimulus_intent);
        Intent lesson_updated_intent = new Intent(UPDATED_FOLLOWING_LESSON);
        lesson_updated_intent.putExtra("lesson", lesson.id);
        service.sendBroadcast(lesson_updated_intent);
    }

    public void removeStimulus(final Message.Stimulus stimulus) {
        int pos = stimuli.indexOf(stimulus);
        stimuli.remove(pos);
        // we remove the stimulus from all the received stimuli..
        service.removeStimulus(stimulus);
        Intent removed_stimulus_intent = new Intent(REMOVED_LESSON_STIMULUS + lesson.id);
        removed_stimulus_intent.putExtra("position", pos);
        service.sendBroadcast(removed_stimulus_intent);
        Intent lesson_updated_intent = new Intent(UPDATED_FOLLOWING_LESSON);
        lesson_updated_intent.putExtra("lesson", lesson.id);
        service.sendBroadcast(lesson_updated_intent);
    }
}
