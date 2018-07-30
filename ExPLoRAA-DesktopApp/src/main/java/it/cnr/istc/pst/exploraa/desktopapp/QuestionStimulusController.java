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
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.ResourceBundle;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Button;
import javafx.scene.control.RadioButton;
import javafx.scene.control.ToggleGroup;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.Priority;
import javafx.scene.web.WebView;

/**
 *
 * @author Riccardo De Benedictis
 */
public class QuestionStimulusController implements Initializable {

    @FXML
    private GridPane root;
    @FXML
    private WebView web_view;
    private final ObjectProperty<Message.Stimulus.QuestionStimulus> stimulus = new SimpleObjectProperty<>();
    private final ToggleGroup group = new ToggleGroup();
    private final List<RadioButton> answer_buttons = new ArrayList<>();
    private final Button send_answer = new Button(Context.LANGUAGE.getString("SEND"));

    @Override
    public void initialize(URL location, ResourceBundle resources) {
        stimulus.addListener((ObservableValue<? extends Message.Stimulus.QuestionStimulus> observable, Message.Stimulus.QuestionStimulus oldValue, Message.Stimulus.QuestionStimulus newValue) -> {
            group.getToggles().clear();
            root.getChildren().remove(send_answer);
            root.getChildren().removeAll(answer_buttons);
            answer_buttons.clear();
            if (newValue != null) {
                web_view.getEngine().loadContent(newValue.question);
                for (int i = 0; i < newValue.answers.size(); i++) {
                    RadioButton ans = new RadioButton(newValue.answers.get(i));
                    ans.setToggleGroup(group);
                    GridPane.setHgrow(ans, Priority.ALWAYS);
                    if (newValue.answer != null) {
                        ans.disableProperty().set(true);
                    }
                    answer_buttons.add(ans);
                    root.add(ans, 0, i + 1);
                }
                if (newValue.answer != null) {
                    group.selectToggle(group.getToggles().get(newValue.answer));
                } else {
                    send_answer.disableProperty().bind(group.selectedToggleProperty().isNull());
                    root.add(send_answer, 0, newValue.answers.size() + 1);
                }
            } else {
                web_view.getEngine().loadContent("");
            }
        });

        send_answer.setOnAction((ActionEvent event1) -> {
            Context.getContext().answerQuestion(stimulus.get(), group.getToggles().indexOf(group.getSelectedToggle()));
            send_answer.disableProperty().unbind();
            send_answer.disableProperty().set(true);
            answer_buttons.forEach(btn -> btn.disableProperty().set(true));
        });
    }

    public ObjectProperty<Message.Stimulus.QuestionStimulus> stimulusProperty() {
        return stimulus;
    }
}
