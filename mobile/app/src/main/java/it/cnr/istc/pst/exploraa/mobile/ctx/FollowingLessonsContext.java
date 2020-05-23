package it.cnr.istc.pst.exploraa.mobile.ctx;

import android.util.Log;

import androidx.annotation.NonNull;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import it.cnr.istc.pst.exploraa.api.Lesson;
import it.cnr.istc.pst.exploraa.api.Message.Stimulus;

public class FollowingLessonsContext {

    private static final FollowingLessonsContext instance = new FollowingLessonsContext();
    /**
     * The lessons followed as a student.
     */
    private final Map<Long, FollowingLessonContext> lessons = new HashMap<>();
    private final List<FollowingLessonsListener> listeners = new ArrayList<>();

    private FollowingLessonsContext() {
        Log.i(StudentsContext.class.getName(), "Creating following lessons context..");
    }

    public static FollowingLessonsContext getInstance() {
        return instance;
    }

    public FollowingLessonContext addLesson(@NonNull Lesson lesson) {
        FollowingLessonContext lesson_ctx = lessons.get(lesson.getId());
        if (lesson_ctx == null) {
            lesson_ctx = new FollowingLessonContext(lesson);
            lessons.put(lesson.getId(), lesson_ctx);
            for (Stimulus stimulus : lesson.getStimuli())
                lesson_ctx.addStimulus(stimulus);
            for (FollowingLessonsListener l : listeners) l.followingLessonAdded(lesson_ctx);
        }
        return lesson_ctx;
    }

    public void removeLesson(long id) {
        final FollowingLessonContext lesson_ctx = lessons.remove(id);
        if (lesson_ctx != null) {
            lesson_ctx.clear();
            for (FollowingLessonsListener l : listeners) l.followingLessonRemoved(lesson_ctx);
        }
    }

    public void clear() {
        for (FollowingLessonContext lesson_ctx : lessons.values())
            lesson_ctx.clear();
        lessons.clear();
        for (FollowingLessonsListener l : listeners) l.followingLessonsCleared();
    }

    public void addListener(@NonNull FollowingLessonsListener l) {
        listeners.add(l);
    }

    public void removeListener(@NonNull FollowingLessonsListener l) {
        listeners.remove(l);
    }

    public interface FollowingLessonsListener {

        void followingLessonAdded(@NonNull FollowingLessonContext lesson);

        void followingLessonRemoved(@NonNull FollowingLessonContext lesson);

        void followingLessonsCleared();
    }

    public static class FollowingLessonContext {

        private final Lesson lesson;
        /**
         * The stimuli received so far.
         */
        private final List<Stimulus> stimuli = new ArrayList<>();
        private final Collection<FollowingLessonListener> listeners = new ArrayList<>();

        public FollowingLessonContext(Lesson lesson) {
            this.lesson = lesson;
            stimuli.addAll(lesson.getStimuli());
        }

        public Lesson getLesson() {
            return lesson;
        }

        public void addStimulus(@NonNull final Stimulus stimulus) {
            stimuli.add(stimulus);
            StimuliContext.getInstance().addStimulus(stimulus);
            for (FollowingLessonListener listener : listeners)
                listener.stimulusAdded(stimulus);
        }

        public void removeStimulus(@NonNull final Stimulus stimulus) {
            stimuli.remove(stimulus);
            StimuliContext.getInstance().removeStimulus(stimulus);
            for (FollowingLessonListener listener : listeners)
                listener.stimulusRemoved(stimulus);
        }

        public List<Stimulus> getStimuli() {
            return Collections.unmodifiableList(stimuli);
        }

        public void clear() {
            for (Stimulus stimulus : stimuli)
                StimuliContext.getInstance().removeStimulus(stimulus);
            stimuli.clear();
            for (FollowingLessonListener listener : listeners)
                listener.stimuliCleared();
        }

        public void addFollowingLessonListener(FollowingLessonListener l) {
            listeners.add(l);
        }

        public void removeFollowingLessonListener(FollowingLessonListener l) {
            listeners.remove(l);
        }

        public interface FollowingLessonListener {

            void stimulusAdded(Stimulus stimulus);

            void stimulusRemoved(Stimulus stimulus);

            void stimuliCleared();
        }
    }
}
