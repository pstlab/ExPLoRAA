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

import javafx.beans.binding.Bindings;
import javafx.scene.control.ButtonBar;
import javafx.scene.control.ButtonType;
import javafx.scene.control.Dialog;
import javafx.scene.control.Label;
import javafx.scene.control.PasswordField;
import javafx.scene.control.TextField;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.Region;
import javafx.stage.Stage;

/**
 *
 * @author Riccardo De Benedictis
 */
public class LoginDialog extends Dialog<LoginDialog.LoginResult> {

    private final GridPane grid = new GridPane();
    private final TextField email_field = new TextField();
    private final PasswordField password_field = new PasswordField();
    private final ButtonType login_button = new ButtonType(Context.LANGUAGE.getString("LOGIN"), ButtonBar.ButtonData.OK_DONE);

    public LoginDialog() {
        setTitle(Context.LANGUAGE.getString("LOGIN"));

        grid.setHgap(10);
        grid.setVgap(10);
        grid.add(new Label(Context.LANGUAGE.getString("E-MAIL") + ":"), 0, 0);
        email_field.setPromptText(Context.LANGUAGE.getString("E-MAIL"));
        grid.add(email_field, 1, 0);
        grid.add(new Label(Context.LANGUAGE.getString("PASSWORD") + ":"), 0, 1);
        password_field.setPromptText(Context.LANGUAGE.getString("PASSWORD"));
        grid.add(password_field, 1, 1);
        getDialogPane().setContent(grid);

        getDialogPane().getButtonTypes().add(login_button);
        getDialogPane().lookupButton(login_button).disableProperty().bind(Bindings.or(email_field.textProperty().isEmpty(), password_field.textProperty().isEmpty()));
        getDialogPane().setMinHeight(Region.USE_PREF_SIZE);
        ((Stage) getDialogPane().getScene().getWindow()).getIcons().addAll(Context.getContext().getStage().getIcons());
        setResultConverter((ButtonType param) -> param == login_button ? new LoginResult(email_field.getText(), password_field.getText()) : null);
    }

    public static class LoginResult {

        private final String email;
        private final String password;

        private LoginResult(String email, String password) {
            this.email = email;
            this.password = password;
        }

        public String getEmail() {
            return email;
        }

        public String getPassword() {
            return password;
        }
    }
}
