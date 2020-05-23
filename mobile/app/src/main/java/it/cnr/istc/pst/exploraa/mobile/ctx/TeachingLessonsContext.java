package it.cnr.istc.pst.exploraa.mobile.ctx;

import android.util.Log;

import androidx.annotation.NonNull;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import it.cnr.istc.pst.exploraa.api.Following;
import it.cnr.istc.pst.exploraa.api.Lesson;
import it.cnr.istc.pst.exploraa.api.Message.Token;

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
            for (TeachingLessonsListener l : listeners) l.teachingLessonAdded(lesson_ctx);
        }
        return lesson_ctx;
    }

    public void removeLesson(long id) {
        final TeachingLessonContext lesson_ctx = lessons.remove(id);
        if (lesson_ctx != null) {
            for (TeachingLessonsListener l : listeners) l.teachingLessonRemoved(lesson_ctx);
        }
    }

    public void clear() {
        for (TeachingLessonContext lesson_ctx : lessons.values())
            lesson_ctx.clear();
        lessons.clear();
        for (TeachingLessonsListener l : listeners) l.teachingLessonsCleared();
    }

    public void addListener(@NonNull TeachingLessonsListener l) {
        listeners.add(l);
    }

    public void removeListener(@NonNull TeachingLessonsListener l) {
        listeners.remove(l);
    }

    public interface TeachingLessonsListener {

        void teachingLessonAdded(@NonNull TeachingLessonContext lesson);

        void teachingLessonRemoved(@NonNull TeachingLessonContext lesson);

        void teachingLessonsCleared();
    }

    public static class TeachingLessonContext {

        private final Lesson lesson;
        /**
         * The tokens received so far.
         */
        private final List<Token> tokens = new ArrayList<>();
        private final Collection<TeachingLessonListener> listeners = new ArrayList<>();

        public TeachingLessonContext(Lesson lesson) {
            this.lesson = lesson;
        }

        public Lesson getLesson() {
            return lesson;
        }

        public void addToken(@NonNull final Token token) {
            tokens.add(token);
            TokensContext.getInstance().addToken(token);
            for (TeachingLessonListener listener : listeners)
                listener.tokenAdded(token);
        }

        public void removeToken(@NonNull final Token token) {
            tokens.remove(token);
            TokensContext.getInstance().removeToken(token);
            for (TeachingLessonListener listener : listeners)
                listener.tokenRemoved(token);
        }

        public List<Token> getTokens() {
            return Collections.unmodifiableList(tokens);
        }

        public void clear() {
            for (Token token : tokens)
                TokensContext.getInstance().removeToken(token);
            tokens.clear();
            for (TeachingLessonListener listener : listeners)
                listener.tokensCleared();
        }

        public void addTeachingLessonListener(@NonNull TeachingLessonListener l) {
            listeners.add(l);
        }

        public void removeTeachingLessonListener(@NonNull TeachingLessonListener l) {
            listeners.remove(l);
        }

        public interface TeachingLessonListener {

            void tokenAdded(Token token);

            void tokenRemoved(Token token);

            void tokensCleared();
        }
    }
}
