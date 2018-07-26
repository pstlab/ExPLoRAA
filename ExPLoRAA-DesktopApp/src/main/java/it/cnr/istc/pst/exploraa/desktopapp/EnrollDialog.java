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
import java.util.Collections;
import java.util.Set;
import java.util.stream.Collectors;
import javafx.beans.binding.Bindings;
import javafx.beans.property.SimpleListProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.control.ButtonBar;
import javafx.scene.control.ButtonType;
import javafx.scene.control.Dialog;
import javafx.scene.control.Label;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.Region;
import javafx.scene.paint.Color;
import javafx.stage.Stage;
import org.controlsfx.control.CheckListView;
import org.controlsfx.glyphfont.FontAwesome;
import org.controlsfx.glyphfont.Glyph;

/**
 *
 * @author Riccardo De Benedictis
 */
public class EnrollDialog extends Dialog<EnrollDialog.EnrollResult> {

    private final GridPane grid = new GridPane();
    private final ObservableList<Lesson> lessons = FXCollections.observableArrayList();
    private ListView<Lesson> lessons_list_view = new ListView<>(lessons);
    private final ObservableList<String> topics = FXCollections.observableArrayList();
    private CheckListView<String> topics_list_view = new CheckListView<>(topics);
    private final ButtonType enroll_button = new ButtonType(Context.LANGUAGE.getString("ENROLL"), ButtonBar.ButtonData.OK_DONE);

    public EnrollDialog() {
        setTitle(Context.LANGUAGE.getString("ENROLL"));

        grid.setHgap(10);
        grid.setVgap(10);
        grid.add(new Label(Context.LANGUAGE.getString("LESSONS")), 0, 0);
        grid.add(lessons_list_view, 0, 1, 1, 1);
        grid.add(new Label(Context.LANGUAGE.getString("TOPICS")), 1, 0);
        grid.add(topics_list_view, 1, 1, 1, 1);
        getDialogPane().setContent(grid);

        lessons.setAll(Context.getContext().getLessons());

        lessons_list_view.setCellFactory((ListView<Lesson> param) -> new ListCell<Lesson>() {
            @Override
            protected void updateItem(Lesson l, boolean empty) {
                super.updateItem(l, empty);
                if (empty) {
                    setText(null);
                    setGraphic(null);
                } else {
                    setText(l.name);
                    switch (l.state) {
                        case Running:
                            setGraphic(new Glyph("FontAwesome", FontAwesome.Glyph.PLAY).color(Color.INDIGO));
                            break;
                        case Paused:
                            setGraphic(new Glyph("FontAwesome", FontAwesome.Glyph.PAUSE).color(Color.INDIGO));
                            break;
                        case Stopped:
                            setGraphic(new Glyph("FontAwesome", FontAwesome.Glyph.STOP).color(Color.INDIGO));
                            break;
                        default:
                            throw new AssertionError(l.state.name());
                    }
                }
            }
        });
        lessons_list_view.selectionModelProperty().get().selectedItemProperty().addListener((ObservableValue<? extends Lesson> observable, Lesson oldValue, Lesson newValue) -> {
            if (newValue != null) {
                topics.setAll(newValue.topics);
            }
        });

        getDialogPane().getButtonTypes().add(enroll_button);
        SimpleListProperty<Integer> selected_indices = new SimpleListProperty<>(topics_list_view.getCheckModel().getCheckedIndices());
        getDialogPane().lookupButton(enroll_button).disableProperty().bind(Bindings.or(lessons_list_view.getSelectionModel().selectedItemProperty().isNotNull(), selected_indices.emptyProperty()));
        getDialogPane().setMinHeight(Region.USE_PREF_SIZE);
        ((Stage) getDialogPane().getScene().getWindow()).getIcons().addAll(Context.getContext().getStage().getIcons());
        setResultConverter((ButtonType param) -> param == enroll_button ? new EnrollResult(lessons_list_view.getSelectionModel().getSelectedItem(), selected_indices.stream().map(i -> topics.get(i)).collect(Collectors.toSet())) : null);
    }

    public static class EnrollResult {

        private final Lesson lesson;
        private final Set<String> interests;

        public EnrollResult(Lesson lesson, Set<String> interests) {
            this.lesson = lesson;
            this.interests = interests;
        }

        public Lesson getLesson() {
            return lesson;
        }

        public Set<String> getInterests() {
            return Collections.unmodifiableSet(interests);
        }
    }
}
