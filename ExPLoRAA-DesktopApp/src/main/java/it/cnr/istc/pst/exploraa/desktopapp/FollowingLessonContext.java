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
import it.cnr.istc.pst.exploraa.api.Message;
import javafx.beans.property.LongProperty;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleLongProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.collections.FXCollections;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;

/**
 *
 * @author Riccardo De Benedictis
 */
public class FollowingLessonContext {

    private final Lesson lesson;
    private final ObjectProperty<LessonState> state = new SimpleObjectProperty<>(LessonState.Stopped);
    private final LongProperty time = new SimpleLongProperty(0);
    private final ObservableList<Message.Stimulus> stimuli = FXCollections.observableArrayList();

    FollowingLessonContext(Lesson lesson) {
        this.lesson = lesson;
        stimuli.addListener((ListChangeListener.Change<? extends Message.Stimulus> c) -> {
            while (c.next()) {
                Context.getContext().stimuliProperty().addAll(c.getAddedSubList());
                Context.getContext().stimuliProperty().removeAll(c.getRemoved());
            }
        });
    }

    public Lesson getLesson() {
        return lesson;
    }

    public ObjectProperty<LessonState> stateProperty() {
        return state;
    }

    public LongProperty timeProperty() {
        return time;
    }

    public ObservableList<Message.Stimulus> stimuliProperty() {
        return stimuli;
    }
}
