package it.cnr.istc.pst.exploraa.mobile.ctx;

import android.util.Log;

import androidx.annotation.NonNull;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import it.cnr.istc.pst.exploraa.api.Following;
import it.cnr.istc.pst.exploraa.api.Lesson;

public class TeachingLessonsContext {

    private static final TeachingLessonsContext instance = new TeachingLessonsContext();
    /**
     * The lessons followed as a teacher.
     */
    private final Map<Long, TeachingLessonContext> lessons = new HashMap<>();
    private final List<TeachingLessonsListener> listeners = new ArrayList<>();

    private TeachingLessonsContext() {
        Log.i(StudentsContext.class.getName(), "Creating teaching lessons context..");
    }

    public static TeachingLessonsContext getInstance() {
        return instance;
    }

    public TeachingLessonContext addLesson(@NonNull Lesson lesson) {
        TeachingLessonContext lesson_ctx = lessons.get(lesson.getId());
        if (lesson_ctx == null) {
            lesson_ctx = new TeachingLessonContext(lesson);
            lessons.put(lesson.getId(), lesson_ctx);
            for (Following following : lesson.getStudents().values())
                StudentsContext.getInstance().addStudent(following.getUser());
            for (TeachingLessonsListener l : listeners) l.lessonAdded(lesson_ctx);
        }
        return lesson_ctx;
    }

    public void removeLesson(Lesson lesson) {
    }

    public void addListener(@NonNull TeachingLessonsListener l) {
        listeners.add(l);
    }

    public void removeListener(@NonNull TeachingLessonsListener l) {
        listeners.remove(l);
    }

    public interface TeachingLessonsListener {

        void lessonAdded(@NonNull TeachingLessonContext lesson);

        void lessonRemoved(@NonNull TeachingLessonContext lesson);
    }

    public static class TeachingLessonContext {

        private final Lesson lesson;

        public TeachingLessonContext(Lesson lesson) {
            this.lesson = lesson;
        }
    }
}
