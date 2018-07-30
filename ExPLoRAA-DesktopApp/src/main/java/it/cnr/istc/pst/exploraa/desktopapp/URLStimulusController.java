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
import java.util.ResourceBundle;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.web.WebView;

/**
 *
 * @author Riccardo De Benedictis
 */
public class URLStimulusController implements Initializable {

    @FXML
    private WebView web_view;
    private final ObjectProperty<Message.Stimulus.URLStimulus> stimulus = new SimpleObjectProperty<>();

    @Override
    public void initialize(URL location, ResourceBundle resources) {
        stimulus.addListener((ObservableValue<? extends Message.Stimulus.URLStimulus> observable, Message.Stimulus.URLStimulus oldValue, Message.Stimulus.URLStimulus newValue) -> {
            if (newValue != null) {
                web_view.getEngine().load(newValue.url);
            } else {
                web_view.getEngine().loadContent("");
            }
        });
    }

    public ObjectProperty<Message.Stimulus.URLStimulus> stimulusProperty() {
        return stimulus;
    }
}
