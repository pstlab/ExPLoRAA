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

import it.cnr.istc.pst.exploraa.api.LessonModel;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;
import javafx.event.ActionEvent;
import javafx.scene.control.Button;
import javafx.scene.control.ButtonBar;
import javafx.scene.control.ButtonType;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Dialog;
import javafx.scene.control.Label;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.scene.control.TextField;
import javafx.scene.layout.GridPane;
import static javafx.scene.layout.GridPane.setHgrow;
import javafx.scene.layout.Priority;
import javafx.scene.layout.Region;
import javafx.stage.FileChooser;
import javafx.stage.Stage;
import javafx.util.StringConverter;
import org.controlsfx.glyphfont.FontAwesome;
import org.controlsfx.glyphfont.Glyph;

/**
 *
 * @author Riccardo De Benedictis
 */
public class AddLessonDialog extends Dialog<AddLessonDialog.AddLessonResult> {

    private static final FileChooser FILE_CHOOSER = new FileChooser();
    private final GridPane grid = new GridPane();
    private final ComboBox<LessonModel> lesson_types = new ComboBox<>();
    private final TextField lesson_name = new TextField();
    private final Button open_button = new Button("", new Glyph("FontAwesome", FontAwesome.Glyph.FILE_CODE_ALT));
    private final ButtonType add_button = new ButtonType(Context.LANGUAGE.getString("ADD"), ButtonBar.ButtonData.OK_DONE);

    public AddLessonDialog() {
        grid.setHgap(10);
        grid.setVgap(10);
        setHgrow(lesson_types, Priority.ALWAYS);
        setHgrow(lesson_name, Priority.ALWAYS);

        setTitle(Context.LANGUAGE.getString("ADD_LESSON"));

        grid.add(new Label(Context.LANGUAGE.getString("LESSON_TYPE") + ":"), 0, 0);
        lesson_types.setPromptText(Context.LANGUAGE.getString("LESSON_TYPE"));
        lesson_types.setEditable(false);
        lesson_types.setItems(Context.getContext().modelsProperty());

        lesson_types.setCellFactory((ListView<LessonModel> param) -> new ListCell<LessonModel>() {
            @Override
            public void updateItem(LessonModel item, boolean empty) {
                super.updateItem(item, empty);
                if (empty || item == null) {
                    setText(null);
                } else {
                    setText(item.name);
                }
            }
        });
        lesson_types.setConverter(new StringConverter<LessonModel>() {
            @Override
            public String toString(LessonModel object) {
                return object.name;
            }

            @Override
            public LessonModel fromString(String string) {
                throw new UnsupportedOperationException("Not supported yet..");
            }
        });
        grid.add(lesson_types, 1, 0);
        grid.add(open_button, 2, 0);
        grid.add(new Label(Context.LANGUAGE.getString("LESSON_NAME") + ":"), 0, 1);
        lesson_name.setPromptText(Context.LANGUAGE.getString("LESSON_NAME"));
        grid.add(lesson_name, 1, 1, 2, 1);

        getDialogPane().setContent(grid);

        open_button.setOnAction((ActionEvent event) -> {
            FILE_CHOOSER.setTitle(Context.LANGUAGE.getString("OPEN_LESSON_FILE"));
            FILE_CHOOSER.setInitialDirectory(new File(System.getProperty("user.home")));
            FILE_CHOOSER.getExtensionFilters().clear();
            FILE_CHOOSER.getExtensionFilters().addAll(
                    new FileChooser.ExtensionFilter(Context.LANGUAGE.getString("LESSON_MODEL_FILE"), "*.json"),
                    new FileChooser.ExtensionFilter(Context.LANGUAGE.getString("ALL_FILES"), "*.*")
            );
            File lesson_file = FILE_CHOOSER.showOpenDialog(Context.getContext().getStage());
            if (lesson_file != null) {
                try {
                    LessonModel model = Context.JSONB.fromJson(new FileInputStream(lesson_file), LessonModel.class);
                    Context.getContext().modelsProperty().add(model);
                    lesson_types.setValue(model);
                } catch (IOException ex) {
                    Logger.getLogger(AddLessonDialog.class.getName()).log(Level.SEVERE, null, ex);
                }
            }
        });

        getDialogPane().getButtonTypes().add(add_button);
        getDialogPane().lookupButton(add_button).disableProperty().bind(lesson_types.valueProperty().isNull().or(lesson_name.textProperty().isEmpty()));
        getDialogPane().setMinHeight(Region.USE_PREF_SIZE);
        ((Stage) getDialogPane().getScene().getWindow()).getIcons().addAll(Context.getContext().getStage().getIcons());
        setResultConverter((ButtonType param) -> param == add_button ? new AddLessonResult(lesson_types.getValue(), lesson_name.getText()) : null);
    }

    public static class AddLessonResult {

        private final LessonModel model;
        private final String lesson_name;

        private AddLessonResult(LessonModel model, String lesson_name) {
            this.model = model;
            this.lesson_name = lesson_name;
        }

        public LessonModel getModel() {
            return model;
        }

        public String getLessonName() {
            return lesson_name;
        }
    }
}
