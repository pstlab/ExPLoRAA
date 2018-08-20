package it.cnr.istc.exploraa;

import android.util.LongSparseArray;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import it.cnr.istc.exploraa.api.Lesson;
import it.cnr.istc.exploraa.api.Message;

public class TeachingLessonContext {

    private final ExPLoRAAService service;
    private final Lesson lesson;
    private final List<TokenRow> tokens = new ArrayList<>();
    private final LongSparseArray<TokenRow> id_tokens = new LongSparseArray<>();
    private final List<StudentContext> students = new ArrayList<>();
    private final LongSparseArray<StudentContext> id_students = new LongSparseArray<>();
    private final List<TeachingLessonListener> listeners = new ArrayList<>();

    TeachingLessonContext(ExPLoRAAService service, Lesson lesson) {
        this.service = service;
        this.lesson = lesson;
        for (Message.Token tk : lesson.tokens) tokens.add(new TokenRow(tk));
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
            for (TeachingLessonListener l : listeners) l.stateChanged(state);
        }
    }

    public long getTime() {
        return lesson.time;
    }

    public void setTime(long time) {
        if (lesson.time != time) {
            lesson.time = time;
            for (TeachingLessonListener l : listeners) l.timeChanged(time);
        }
    }

    public List<TokenRow> getTokens() {
        return Collections.unmodifiableList(tokens);
    }

    public void addToken(final Message.Token tk) {
        final int pos = tokens.size();
        TokenRow tk_r = new TokenRow(tk);
        tokens.add(tk_r);
        id_tokens.put(tk.id, tk_r);
        for (TeachingLessonListener l : listeners) l.addedToken(pos, tk_r);
    }

    public void removeToken(final Message.Token tk) {
        TokenRow tk_row = id_tokens.get(tk.id);
        final int pos = tokens.indexOf(tk_row);
        tokens.remove(pos);
        id_tokens.remove(tk.id);
        for (TeachingLessonListener l : listeners) l.removedToken(pos, tk_row);
    }

    public TokenRow getToken(int id) {
        return id_tokens.get(id);
    }

    public void updateToken(int id, long time, long min, long max) {
        final TokenRow tk_row = id_tokens.get(id);
        int pos = tokens.indexOf(tk_row);
        tk_row.getToken().time = time;
        tk_row.getToken().min = min;
        tk_row.getToken().max = max;
        for (TeachingLessonListener l : listeners) l.updatedToken(pos, tk_row);
    }

    public void addStudent(StudentContext s_ctx) {
        final int pos = students.size();
        students.add(s_ctx);
        id_students.put(s_ctx.getStudent().id, s_ctx);
        // we add, if needed, this student to all the user's students..
        if (service.getStudent(s_ctx.getStudent().id) == null)
            service.addStudent(s_ctx);
        for (TeachingLessonListener l : listeners) l.studentAdded(pos, s_ctx);
    }

    public void removeStudent(StudentContext s_ctx) {
        final int pos = students.indexOf(s_ctx);
        students.remove(pos);
        id_students.remove(s_ctx.getStudent().id);
        // we remove, if it is not following any other lesson teached by this user, this student from all the user's students..
        Set<Long> c_students = new HashSet<>();
        for (TeachingLessonContext l : service.getTeachingLessons())
            for (StudentContext student : l.getStudents())
                c_students.add(student.getStudent().id);
        if (!c_students.contains(s_ctx.getStudent().id))
            service.removeStudent(id_students.get(s_ctx.getStudent().id));
        for (TeachingLessonListener l : listeners) l.studentRemoved(pos, s_ctx);
    }

    public List<StudentContext> getStudents() {
        return Collections.unmodifiableList(students);
    }

    public void addListener(TeachingLessonListener l) {
        listeners.add(l);
    }

    public void removeListener(TeachingLessonListener l) {
        listeners.remove(l);
    }

    public interface TeachingLessonListener {

        void timeChanged(long t);

        void stateChanged(Lesson.LessonState state);

        void studentAdded(int pos, StudentContext s_ctx);

        void studentRemoved(int pos, StudentContext s_ctx);

        void addedToken(int pos, TokenRow tk);

        void removedToken(int pos, TokenRow tk);

        void updatedToken(int pos, TokenRow tk);
    }

    public static class TokenRow {

        private final Message.Token tk;

        private TokenRow(Message.Token tk) {
            this.tk = tk;
        }

        public Message.Token getToken() {
            return tk;
        }

        public long getTime() {
            return tk.time;
        }

        public Long getMin() {
            return tk.min;
        }

        public Long getMax() {
            return tk.max;
        }
    }
}
