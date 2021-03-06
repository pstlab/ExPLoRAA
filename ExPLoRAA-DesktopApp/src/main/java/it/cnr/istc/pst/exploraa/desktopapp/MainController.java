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

import it.cnr.istc.pst.exploraa.api.Message;
import it.cnr.istc.pst.exploraa.api.User;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.ResourceBundle;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.prefs.Preferences;
import javafx.application.Platform;
import javafx.beans.binding.Bindings;
import javafx.beans.property.ObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ListChangeListener;
import javafx.collections.transformation.SortedList;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.fxml.Initializable;
import javafx.scene.control.Accordion;
import javafx.scene.control.Alert;
import javafx.scene.control.Button;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.scene.control.MenuItem;
import javafx.scene.control.Tab;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.Pane;
import javafx.scene.layout.StackPane;
import javafx.scene.paint.Color;
import javafx.stage.Stage;
import org.controlsfx.control.Notifications;
import org.controlsfx.glyphfont.FontAwesome;
import org.controlsfx.glyphfont.Glyph;

/**
 *
 * @author Riccardo De Benedictis
 */
public class MainController implements Initializable {

    @FXML
    private MenuItem login;
    @FXML
    private MenuItem logout;
    @FXML
    private MenuItem new_user;
    @FXML
    private Tab learn_tab;
    @FXML
    private Accordion learn_accord;
    @FXML
    private ListView<Message.Stimulus> stimuli;
    @FXML
    private ListView<FollowingLessonContext> following_lessons;
    @FXML
    private Button add_following_lessons_button;
    @FXML
    private Button remove_selected_following_lessons_button;
    @FXML
    private StackPane learning_pane;
    private Pane text_stimulus_pane;
    private TextStimulusController text_stimulus_controller;
    private Pane question_stimulus_pane;
    private QuestionStimulusController question_stimulus_controller;
    private Pane url_stimulus_pane;
    private URLStimulusController url_stimulus_controller;
    @FXML
    private Tab teach_tab;
    @FXML
    private Accordion teach_accord;
    @FXML
    private ListView<TeachingLessonContext> teaching_lessons;
    @FXML
    private Button add_teaching_lesson_button;
    @FXML
    private Button remove_selected_teaching_lessons_button;
    @FXML
    private ListView<StudentContext> students;
    @FXML
    private StackPane teaching_pane;
    private Pane lesson_pane;
    private LessonController lesson_controller;
    private Pane student_pane;
    private StudentController student_controller;
    private final Preferences prefs = Preferences.userNodeForPackage(MainController.class);

    @Override
    public void initialize(URL location, ResourceBundle resources) {
        Stage stage = Context.getContext().getStage();
        stage.setTitle("ExPLoRAA (ExPeriential LeaRning for Active Aging)");

        ObjectProperty<User> user = Context.getContext().userProperty();
        user.addListener((ObservableValue<? extends User> observable, User oldValue, User newValue) -> {
            if (newValue != null) {
                stage.setTitle("ExPLoRAA (ExPeriential LeaRning for Active Aging) - " + newValue.first_name);
            } else {
                stage.setTitle("ExPLoRAA (ExPeriential LeaRning for Active Aging)");
                learning_pane.getChildren().clear();
                teaching_pane.getChildren().clear();
            }
        });

        login.disableProperty().bind(user.isNotNull());
        new_user.disableProperty().bind(user.isNotNull());
        logout.disableProperty().bind(user.isNull());

        learn_tab.setGraphic(new Glyph("FontAwesome", FontAwesome.Glyph.BOOK).size(20));
        learn_accord.setExpandedPane(learn_accord.getPanes().get(0));

        stimuli.setItems(new SortedList<>(Context.getContext().stimuliProperty(), (Message.Stimulus e0, Message.Stimulus e1) -> Long.compare(e0.time, e1.time)));
        stimuli.setCellFactory((ListView<Message.Stimulus> param) -> new ListCell<Message.Stimulus>() {
            @Override
            protected void updateItem(Message.Stimulus event, boolean empty) {
                super.updateItem(event, empty);
                if (empty) {
                    setText(null);
                    setGraphic(null);
                } else {
                    if (event instanceof Message.Stimulus.TextStimulus) {
                        setText(((Message.Stimulus.TextStimulus) event).content);
                        setGraphic(new Glyph("FontAwesome", FontAwesome.Glyph.INFO));
                    } else if (event instanceof Message.Stimulus.URLStimulus) {
                        setText(((Message.Stimulus.URLStimulus) event).content);
                        setGraphic(new Glyph("FontAwesome", FontAwesome.Glyph.INFO));
                    } else if (event instanceof Message.Stimulus.QuestionStimulus) {
                        setText(((Message.Stimulus.QuestionStimulus) event).question);
                        setGraphic(new Glyph("FontAwesome", FontAwesome.Glyph.QUESTION));
                    }
                }
            }
        });
        Context.getContext().stimuliProperty().addListener((ListChangeListener.Change<? extends Message.Stimulus> c) -> {
            while (c.next()) {
                for (Message.Stimulus event : c.getAddedSubList()) {
                    switch (event.stimulus_type) {
                        case Text:
                            Platform.runLater(() -> Notifications.create().title(Context.LANGUAGE.getString("STIMULUS")).text(((Message.Stimulus.TextStimulus) event).content).show());
                            break;
                        case Question:
                            Platform.runLater(() -> Notifications.create().title(Context.LANGUAGE.getString("QUESTION")).text(((Message.Stimulus.QuestionStimulus) event).question).show());
                            break;
                        case URL:
                            Platform.runLater(() -> Notifications.create().title(Context.LANGUAGE.getString("STIMULUS")).text(((Message.Stimulus.URLStimulus) event).content).show());
                            break;
                        default:
                            throw new AssertionError(event.stimulus_type.name());
                    }
                }
            }
        });

        following_lessons.setItems(Context.getContext().followingLessonsProperty());
        following_lessons.setCellFactory((ListView<FollowingLessonContext> param) -> new ListCell<FollowingLessonContext>() {
            @Override
            protected void updateItem(FollowingLessonContext l_ctx, boolean empty) {
                super.updateItem(l_ctx, empty);
                if (empty) {
                    setText(null);
                    setGraphic(null);
                } else {
                    setText(l_ctx.getLesson().name);
                    switch (l_ctx.stateProperty().get()) {
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
                            throw new AssertionError(l_ctx.stateProperty().get().name());
                    }
                }
            }
        });
        add_following_lessons_button.graphicProperty().set(new Glyph("FontAwesome", FontAwesome.Glyph.PLUS));
        add_following_lessons_button.disableProperty().bind(user.isNull());
        remove_selected_following_lessons_button.graphicProperty().set(new Glyph("FontAwesome", FontAwesome.Glyph.MINUS));
        remove_selected_following_lessons_button.disableProperty().bind(Bindings.isEmpty(following_lessons.selectionModelProperty().get().getSelectedItems()));

        try {
            FXMLLoader text_stimulus_pane_loader = new FXMLLoader(getClass().getResource("/fxml/TextStimulus.fxml"));
            text_stimulus_pane = text_stimulus_pane_loader.load();
            text_stimulus_controller = text_stimulus_pane_loader.getController();
            FXMLLoader question_stimulus_pane_loader = new FXMLLoader(getClass().getResource("/fxml/QuestionStimulus.fxml"));
            question_stimulus_pane = question_stimulus_pane_loader.load();
            question_stimulus_controller = question_stimulus_pane_loader.getController();
            FXMLLoader url_stimulus_pane_loader = new FXMLLoader(getClass().getResource("/fxml/URLStimulus.fxml"));
            url_stimulus_pane = url_stimulus_pane_loader.load();
            url_stimulus_controller = url_stimulus_pane_loader.getController();
        } catch (IOException ex) {
            Logger.getLogger(MainController.class.getName()).log(Level.SEVERE, null, ex);
        }
        stimuli.getSelectionModel().selectedItemProperty().addListener((ObservableValue<? extends Message.Stimulus> observable, Message.Stimulus oldValue, Message.Stimulus newValue) -> {
            if (newValue != null) {
                Pane c_pane = null;
                switch (newValue.stimulus_type) {
                    case Text:
                        c_pane = text_stimulus_pane;
                        text_stimulus_controller.stimulusProperty().set((Message.Stimulus.TextStimulus) newValue);
                        break;
                    case Question:
                        c_pane = question_stimulus_pane;
                        question_stimulus_controller.stimulusProperty().set((Message.Stimulus.QuestionStimulus) newValue);
                        break;
                    case URL:
                        c_pane = url_stimulus_pane;
                        url_stimulus_controller.stimulusProperty().set((Message.Stimulus.URLStimulus) newValue);
                        break;
                    default:
                        throw new AssertionError(newValue.stimulus_type.name());
                }
                if (learning_pane.getChildren().isEmpty()) {
                    learning_pane.getChildren().add(c_pane);
                } else if (learning_pane.getChildren().get(0) != c_pane) {
                    learning_pane.getChildren().set(0, c_pane);
                }
            } else {
                text_stimulus_controller.stimulusProperty().set(null);
                url_stimulus_controller.stimulusProperty().set(null);
            }
        });

        teach_tab.setGraphic(new Glyph("FontAwesome", FontAwesome.Glyph.BULLHORN).size(20));
        teach_accord.setExpandedPane(teach_accord.getPanes().get(0));

        teaching_lessons.setItems(Context.getContext().teachingLessonsProperty());
        teaching_lessons.setCellFactory((ListView<TeachingLessonContext> param) -> new ListCell<TeachingLessonContext>() {
            @Override
            protected void updateItem(TeachingLessonContext l_ctx, boolean empty) {
                super.updateItem(l_ctx, empty);
                if (empty) {
                    setText(null);
                    setGraphic(null);
                } else {
                    setText(l_ctx.getLesson().name);
                    switch (l_ctx.stateProperty().get()) {
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
                            throw new AssertionError(l_ctx.stateProperty().get().name());
                    }
                }
            }
        });
        add_teaching_lesson_button.graphicProperty().set(new Glyph("FontAwesome", FontAwesome.Glyph.PLUS));
        add_teaching_lesson_button.disableProperty().bind(user.isNull());
        remove_selected_teaching_lessons_button.graphicProperty().set(new Glyph("FontAwesome", FontAwesome.Glyph.MINUS));
        remove_selected_teaching_lessons_button.disableProperty().bind(Bindings.isEmpty(teaching_lessons.selectionModelProperty().get().getSelectedItems()));

        try {
            FXMLLoader lesson_pane_loader = new FXMLLoader(getClass().getResource("/fxml/Lesson.fxml"), Context.LANGUAGE);
            lesson_pane = lesson_pane_loader.load();
            lesson_controller = lesson_pane_loader.getController();
        } catch (IOException ex) {
            Logger.getLogger(MainController.class.getName()).log(Level.SEVERE, null, ex);
        }

        teaching_lessons.getSelectionModel().selectedItemProperty().addListener((ObservableValue<? extends TeachingLessonContext> observable, TeachingLessonContext oldValue, TeachingLessonContext newValue) -> {
            lesson_controller.teachingLessonContextProperty().set(newValue);
            if (newValue != null) {
                if (teaching_pane.getChildren().isEmpty()) {
                    teaching_pane.getChildren().add(lesson_pane);
                } else if (teaching_pane.getChildren().get(0) != lesson_pane) {
                    teaching_pane.getChildren().set(0, lesson_pane);
                }
            }
        });
        teaching_lessons.setOnMouseClicked((MouseEvent event) -> {
            lesson_controller.teachingLessonContextProperty().set(teaching_lessons.getSelectionModel().getSelectedItem());
            if (teaching_lessons.getSelectionModel().getSelectedItem() != null) {
                if (teaching_pane.getChildren().isEmpty()) {
                    teaching_pane.getChildren().add(lesson_pane);
                } else if (teaching_pane.getChildren().get(0) != lesson_pane) {
                    teaching_pane.getChildren().set(0, lesson_pane);
                }
            }
        });

        students.setItems(Context.getContext().studentsProperty());
        students.setCellFactory((ListView<StudentContext> param) -> new ListCell<StudentContext>() {
            @Override
            protected void updateItem(StudentContext std_ctx, boolean empty) {
                super.updateItem(std_ctx, empty);
                if (empty) {
                    setText(null);
                    setGraphic(null);
                } else {
                    setText(std_ctx.getStudent().first_name + " " + std_ctx.getStudent().last_name);
                    if (std_ctx.isOnline()) {
                        setStyle("-fx-text-fill: black;");
                        setGraphic(new Glyph("FontAwesome", FontAwesome.Glyph.LINK));
                    } else {
                        setStyle("-fx-text-fill: gray;");
                        setGraphic(new Glyph("FontAwesome", FontAwesome.Glyph.UNLINK));
                    }
                }
            }
        });

        try {
            FXMLLoader student_pane_loader = new FXMLLoader(getClass().getResource("/fxml/Student.fxml"), Context.LANGUAGE);
            student_pane = student_pane_loader.load();
            student_controller = student_pane_loader.getController();
        } catch (IOException ex) {
            Logger.getLogger(MainController.class.getName()).log(Level.SEVERE, null, ex);
        }

        students.getSelectionModel().selectedItemProperty().addListener((ObservableValue<? extends StudentContext> observable, StudentContext oldValue, StudentContext newValue) -> {
            student_controller.studentContextProperty().set(newValue);
            if (newValue != null) {
                if (teaching_pane.getChildren().isEmpty()) {
                    teaching_pane.getChildren().add(student_pane);
                } else if (teaching_pane.getChildren().get(0) != student_pane) {
                    teaching_pane.getChildren().set(0, student_pane);
                }
            }
        });
        students.setOnMouseClicked((MouseEvent event) -> {
            student_controller.studentContextProperty().set(students.getSelectionModel().getSelectedItem());
            if (students.getSelectionModel().getSelectedItem() != null) {
                if (teaching_pane.getChildren().isEmpty()) {
                    teaching_pane.getChildren().add(student_pane);
                } else if (teaching_pane.getChildren().get(0) != student_pane) {
                    teaching_pane.getChildren().set(0, student_pane);
                }
            }
        });

        try {
            if (prefs.get("email", null) != null && prefs.get("password", null) != null) {
                Context.getContext().login(prefs.get("email", null), prefs.get("password", null));
            }
        } catch (Exception ex) {
            Alert alert = new Alert(Alert.AlertType.ERROR);
            alert.setTitle(Context.LANGUAGE.getString("EXCEPTION"));
            alert.setHeaderText(ex.getMessage());
            alert.showAndWait();
        }
    }

    @FXML
    private void login(ActionEvent event) {
        LoginDialog login_dialog = new LoginDialog();
        login_dialog.getDialogPane().getStylesheets().addAll(Context.getContext().getStage().getScene().getStylesheets());
        login_dialog.showAndWait().ifPresent(user -> {
            try {
                Context.getContext().login(user.getEmail(), user.getPassword());
                prefs.put("email", user.getEmail());
                prefs.put("password", user.getPassword());
            } catch (Exception e) {
                Alert alert = new Alert(Alert.AlertType.ERROR);
                alert.setTitle(Context.LANGUAGE.getString("EXCEPTION"));
                alert.setHeaderText(e.getMessage());
                alert.showAndWait();
            }
        });
    }

    @FXML
    private void logout(ActionEvent event) {
        Context.getContext().logout();
        prefs.remove("email");
        prefs.remove("password");
    }

    @FXML
    private void new_user(ActionEvent event) {
        NewUserDialog new_user_dialog = new NewUserDialog();
        new_user_dialog.getDialogPane().getStylesheets().addAll(Context.getContext().getStage().getScene().getStylesheets());
        new_user_dialog.showAndWait().ifPresent(user -> {
            try {
                Context.getContext().new_user(user.getEmail(), user.getPassword(), user.getFirstName(), user.getLastName());
                prefs.put("email", user.getEmail());
                prefs.put("password", user.getPassword());
            } catch (Exception e) {
                Alert alert = new Alert(Alert.AlertType.ERROR);
                alert.setTitle(Context.LANGUAGE.getString("EXCEPTION"));
                alert.setHeaderText(e.getMessage());
                alert.showAndWait();
            }
        });
    }

    @FXML
    private void exit(ActionEvent event) {
        Platform.exit();
    }

    @FXML
    private void add_following_lessons(ActionEvent event) {
        EnrollDialog enroll_dialog = new EnrollDialog();
        enroll_dialog.getDialogPane().getStylesheets().addAll(Context.getContext().getStage().getScene().getStylesheets());
        enroll_dialog.showAndWait().ifPresent(enroll_lesson -> {
            Context.getContext().followLesson(enroll_lesson.getLesson(), enroll_lesson.getInterests());
        });
    }

    @FXML
    private void remove_selected_following_lessons(ActionEvent event) {
        for (FollowingLessonContext lesson : new ArrayList<>(following_lessons.getSelectionModel().getSelectedItems())) {
            Context.getContext().unfollowLesson(lesson.getLesson());
        }
    }

    @FXML
    private void add_teaching_lesson(ActionEvent event) {
        new AddLessonDialog().showAndWait().ifPresent(new_lesson -> Context.getContext().addLesson(new_lesson.getLessonName(), new_lesson.getModel()));
    }

    @FXML
    private void remove_selected_teaching_lessons(ActionEvent event) {
        teaching_lessons.selectionModelProperty().get().getSelectedItems().forEach(l_ctx -> Context.getContext().removeLesson(l_ctx));
    }
}
