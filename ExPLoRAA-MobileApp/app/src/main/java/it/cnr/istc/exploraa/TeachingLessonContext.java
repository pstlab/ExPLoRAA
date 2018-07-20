package it.cnr.istc.exploraa;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import it.cnr.istc.exploraa.api.Lesson;
import it.cnr.istc.exploraa.api.LessonModel;
import it.cnr.istc.exploraa.api.Message;

public class TeachingLessonContext {

    private final Lesson lesson;
    private final LessonModel model;
    private final List<TokenRow> tokens = new ArrayList<>();
    private final Map<Integer, TokenRow> id_tokens = new HashMap<>();
    private final List<TeachingLessonListener> listeners = new ArrayList<>();
    private Lesson.LessonState state = Lesson.LessonState.Stopped;
    private long time = 0;

    TeachingLessonContext(Lesson lesson, LessonModel model) {
        this.lesson = lesson;
        this.model = model;
        if (lesson.tokens != null) {
            for (Message.Token tk : lesson.tokens) {
                tokens.add(new TokenRow(tk));
            }
        }
    }

    public Lesson getLesson() {
        return lesson;
    }

    public LessonModel getModel() {
        return model;
    }

    public Lesson.LessonState getState() {
        return state;
    }

    public void setState(Lesson.LessonState state) {
        if (this.state != state) {
            this.state = state;
            for (TeachingLessonListener l : listeners) l.stateChanged(state);
        }
    }

    public long getTime() {
        return time;
    }

    public void setTime(long time) {
        if (this.time != time) {
            this.time = time;
            for (TeachingLessonListener l : listeners) l.timeChanged(time);
        }
    }

    public List<TokenRow> getTokens() {
        return Collections.unmodifiableList(tokens);
    }

    public void addToken(final Message.Token tk) {
        TokenRow tk_r = new TokenRow(tk);
        tokens.add(tk_r);
        id_tokens.put(tk.id, tk_r);
        for (TeachingLessonListener l : listeners) l.addedToken(tk_r);
    }

    public void removeToken(final Message.Token tk) {
        TokenRow tk_r = id_tokens.remove(tk.id);
        tokens.remove(tk_r);
        for (TeachingLessonListener l : listeners) l.removedToken(tk_r);
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

        void addedToken(TokenRow tk);

        void removedToken(TokenRow tk);
    }

    public static class TokenRow {

        private final Message.Token tk;

        private TokenRow(Message.Token tk) {
            this.tk = tk;
        }
    }
}
