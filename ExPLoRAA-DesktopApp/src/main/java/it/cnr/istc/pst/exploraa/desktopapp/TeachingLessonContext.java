/*
 * Copyright (C) 2018 Your Organisation
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package it.cnr.istc.pst.exploraa.desktopapp;

import it.cnr.istc.pst.exploraa.api.Lesson;
import it.cnr.istc.pst.exploraa.api.Lesson.LessonState;
import it.cnr.istc.pst.exploraa.api.LessonModel;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;
import javafx.beans.Observable;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.LongProperty;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleLongProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.collections.FXCollections;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;

/**
 *
 * @author Riccardo De Benedictis
 */
public class TeachingLessonContext {

    private final Lesson lesson;
    private final LessonModel model;
    private final ObjectProperty<LessonState> state = new SimpleObjectProperty<>(LessonState.Stopped);
    private final LongProperty time = new SimpleLongProperty(0);
    private final ObservableList<TokenRow> tokens = FXCollections.observableArrayList((TokenRow tk) -> new Observable[]{tk.timeProperty()});
    private final Map<Integer, TokenRow> id_tokens = new HashMap<>();

    TeachingLessonContext(Lesson lesson, LessonModel model) {
        this.lesson = lesson;
        this.model = model;
        tokens.addListener((ListChangeListener.Change<? extends TokenRow> c) -> {
            while (c.next()) {
                c.getAddedSubList().forEach((tk) -> id_tokens.put(tk.getId(), tk));
                c.getRemoved().forEach((tk) -> id_tokens.remove(tk.getId()));
            }
        });
        if (lesson.tokens != null) {
            tokens.addAll(lesson.tokens.stream().map(token -> new TeachingLessonContext.TokenRow(token.id, time, token.min, token.max, token.time, token.refEvent)).collect(Collectors.toList()));
        }
    }

    public Lesson getLesson() {
        return lesson;
    }

    public LessonModel getModel() {
        return model;
    }

    public ObjectProperty<LessonState> stateProperty() {
        return state;
    }

    public LongProperty timeProperty() {
        return time;
    }

    public ObservableList<TokenRow> tokensProperty() {
        return tokens;
    }

    public TokenRow getToken(final int id) {
        return id_tokens.get(id);
    }

    public static class TokenRow {

        private final int token_id;
        private final BooleanProperty executed;
        private final LongProperty min;
        private final LongProperty max;
        private final LongProperty time;
        private final StringProperty name;

        public TokenRow(int token_id, LongProperty lesson_time, Long min, Long max, long time, String name) {
            this.token_id = token_id;
            this.executed = new SimpleBooleanProperty(false);
            this.min = min != null ? new SimpleLongProperty(min) : new SimpleLongProperty();
            this.max = max != null ? new SimpleLongProperty(max) : new SimpleLongProperty();
            this.time = new SimpleLongProperty(time);
            this.name = new SimpleStringProperty(name);
            executed.bind(lesson_time.greaterThanOrEqualTo(this.time));
        }

        public int getId() {
            return token_id;
        }

        public boolean getExecuted() {
            return executed.get();
        }

        public BooleanProperty executedProperty() {
            return executed;
        }

        public long getMin() {
            return min.get();
        }

        public LongProperty minProperty() {
            return min;
        }

        public long getMax() {
            return max.get();
        }

        public LongProperty maxProperty() {
            return max;
        }

        public long getTime() {
            return time.get();
        }

        public LongProperty timeProperty() {
            return time;
        }

        public String getName() {
            return name.get();
        }

        public StringProperty nameProperty() {
            return name;
        }
    }
}
