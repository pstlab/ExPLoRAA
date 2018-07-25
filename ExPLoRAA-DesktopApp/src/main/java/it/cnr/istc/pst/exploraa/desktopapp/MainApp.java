package it.cnr.istc.pst.exploraa.desktopapp;

import javafx.application.Application;
import static javafx.application.Application.launch;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.image.Image;
import javafx.stage.Stage;

public class MainApp extends Application {

    @Override
    public void start(Stage stage) throws Exception {
        Context.getContext().setStage(stage);
        Parent root = FXMLLoader.load(getClass().getResource("/fxml/Main.fxml"), Context.LANGUAGE);

        Scene scene = new Scene(root);
        scene.getStylesheets().add("/styles/Styles.css");

        stage.getIcons().addAll(new Image(getClass().getResourceAsStream("/imgs/icon_16x16.png")), new Image(getClass().getResourceAsStream("/imgs/icon_32x32.png")), new Image(getClass().getResourceAsStream("/imgs/icon_48x48.png")));
        stage.setScene(scene);
        stage.show();
    }

    @Override
    public void stop() throws Exception {
        Context.getContext().logout();
    }

    /**
     * The main() method is ignored in correctly deployed JavaFX application.
     * main() serves only as fallback in case the application can not be
     * launched through deployment artifacts, e.g., in IDEs with limited FX
     * support. NetBeans ignores main().
     *
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        launch(args);
    }
}
