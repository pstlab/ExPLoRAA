package it.cnr.istc.pst.exploraa.mobile.ctx;

import android.util.Log;

import androidx.annotation.NonNull;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import it.cnr.istc.pst.exploraa.api.Lesson;

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
            for (FollowingLessonsListener l : listeners) l.lessonAdded(lesson_ctx);
        }
        return lesson_ctx;
    }

    public void removeLesson(Lesson lesson) {
    }

    public void addListener(@NonNull FollowingLessonsListener l) {
        listeners.add(l);
    }

    public void removeListener(@NonNull FollowingLessonsListener l) {
        listeners.remove(l);
    }

    public interface FollowingLessonsListener {

        void lessonAdded(@NonNull FollowingLessonContext lesson);

        void lessonRemoved(@NonNull FollowingLessonContext lesson);
    }

    public static class FollowingLessonContext {

        private final Lesson lesson;

        public FollowingLessonContext(Lesson lesson) {
            this.lesson = lesson;
        }
    }
}
