package it.cnr.istc.exploraa;

import android.content.Intent;
import android.util.LongSparseArray;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import it.cnr.istc.exploraa.api.Lesson;
import it.cnr.istc.exploraa.api.Message;

public class TeachingLessonContext {

    public static final String TEACHING_LESSON_STATE_CHANGED = "Teaching lesson state changed";
    public static final String TEACHING_LESSON_TIME_CHANGED = "Teaching lesson time changed";
    public static final String UPDATED_TEACHING_LESSON = "Teaching lesson updated";
    public static final String ADDED_LESSON_TOKEN = "Added lesson token";
    public static final String REMOVED_LESSON_TOKEN = "Removed lesson token";
    public static final String UPDATED_LESSON_TOKEN = "Updated lesson token";
    public static final String ADDED_LESSON_STUDENT = "Added lesson student";
    public static final String REMOVED_LESSON_STUDENT = "Removed lesson student";
    private final ExPLoRAAService service;
    private final Lesson lesson;
    private final List<TokenRow> tokens = new ArrayList<>();
    private final LongSparseArray<TokenRow> id_tokens = new LongSparseArray<>();
    private final List<StudentContext> students = new ArrayList<>();
    private final LongSparseArray<StudentContext> id_students = new LongSparseArray<>();

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
            Intent lesson_state_changed_intent = new Intent(TEACHING_LESSON_STATE_CHANGED + lesson.id);
            lesson_state_changed_intent.putExtra("state", state.name());
            service.sendBroadcast(lesson_state_changed_intent);
            Intent lesson_updated_intent = new Intent(UPDATED_TEACHING_LESSON);
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
            Intent lesson_time_changed_intent = new Intent(TEACHING_LESSON_TIME_CHANGED + lesson.id);
            lesson_time_changed_intent.putExtra("time", time);
            service.sendBroadcast(lesson_time_changed_intent);
            Intent lesson_updated_intent = new Intent(UPDATED_TEACHING_LESSON);
            lesson_updated_intent.putExtra("lesson", lesson.id);
            service.sendBroadcast(lesson_updated_intent);
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
        Intent added_token_intent = new Intent(ADDED_LESSON_TOKEN + lesson.id);
        added_token_intent.putExtra("position", pos);
        service.sendBroadcast(added_token_intent);
        Intent lesson_updated_intent = new Intent(UPDATED_TEACHING_LESSON);
        lesson_updated_intent.putExtra("lesson", lesson.id);
        service.sendBroadcast(lesson_updated_intent);
    }

    public void removeToken(final Message.Token tk) {
        TokenRow tk_row = id_tokens.get(tk.id);
        final int pos = tokens.indexOf(tk_row);
        tokens.remove(pos);
        id_tokens.remove(tk.id);
        Intent removed_token_intent = new Intent(REMOVED_LESSON_TOKEN + lesson.id);
        removed_token_intent.putExtra("position", pos);
        service.sendBroadcast(removed_token_intent);
        Intent lesson_updated_intent = new Intent(UPDATED_TEACHING_LESSON);
        lesson_updated_intent.putExtra("lesson", lesson.id);
        service.sendBroadcast(lesson_updated_intent);
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
        Intent removed_token_intent = new Intent(UPDATED_LESSON_TOKEN + lesson.id);
        removed_token_intent.putExtra("position", pos);
        service.sendBroadcast(removed_token_intent);
        Intent lesson_updated_intent = new Intent(UPDATED_TEACHING_LESSON);
        lesson_updated_intent.putExtra("lesson", lesson.id);
        service.sendBroadcast(lesson_updated_intent);
    }

    public void addStudent(StudentContext s_ctx) {
        final int pos = students.size();
        students.add(s_ctx);
        id_students.put(s_ctx.getStudent().id, s_ctx);
        Intent added_student_intent = new Intent(ADDED_LESSON_STUDENT + lesson.id);
        added_student_intent.putExtra("position", pos);
        service.sendBroadcast(added_student_intent);
        Intent lesson_updated_intent = new Intent(UPDATED_TEACHING_LESSON);
        lesson_updated_intent.putExtra("lesson", lesson.id);
        service.sendBroadcast(lesson_updated_intent);
    }

    public void removeStudent(StudentContext s_ctx) {
        final int pos = students.indexOf(s_ctx);
        students.remove(pos);
        id_students.remove(s_ctx.getStudent().id);
        Intent added_student_intent = new Intent(REMOVED_LESSON_STUDENT + lesson.id);
        added_student_intent.putExtra("position", pos);
        service.sendBroadcast(added_student_intent);
        Intent lesson_updated_intent = new Intent(UPDATED_TEACHING_LESSON);
        lesson_updated_intent.putExtra("lesson", lesson.id);
        service.sendBroadcast(lesson_updated_intent);
    }

    public List<StudentContext> getStudents() {
        return Collections.unmodifiableList(students);
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
