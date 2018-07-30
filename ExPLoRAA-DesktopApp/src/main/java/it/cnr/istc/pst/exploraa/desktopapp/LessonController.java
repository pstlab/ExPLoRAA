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
import it.cnr.istc.pst.exploraa.api.LessonModel;
import it.cnr.istc.pst.exploraa.desktopapp.TeachingLessonContext.TokenRow;
import java.net.URL;
import java.text.FieldPosition;
import java.text.NumberFormat;
import java.text.ParsePosition;
import java.util.ArrayList;
import java.util.List;
import java.util.ResourceBundle;
import java.util.stream.Collectors;
import javafx.animation.Interpolator;
import javafx.animation.KeyFrame;
import javafx.animation.KeyValue;
import javafx.animation.Timeline;
import javafx.beans.binding.Bindings;
import javafx.beans.property.LongProperty;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleLongProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.collections.ListChangeListener;
import javafx.collections.transformation.SortedList;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Button;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.Label;
import javafx.scene.control.MenuItem;
import javafx.scene.control.SeparatorMenuItem;
import javafx.scene.control.TableCell;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableRow;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.control.cell.TextFieldTableCell;
import javafx.scene.layout.StackPane;
import javafx.scene.paint.Color;
import javafx.util.Duration;
import javafx.util.StringConverter;
import javax.swing.UIManager;
import org.controlsfx.glyphfont.FontAwesome;
import org.controlsfx.glyphfont.Glyph;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.axis.NumberAxis;
import org.jfree.chart.fx.ChartViewer;
import org.jfree.chart.plot.ValueMarker;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.XYShapeRenderer;
import org.jfree.chart.ui.RectangleAnchor;
import org.jfree.chart.ui.TextAnchor;
import org.jfree.chart.util.DefaultShadowGenerator;
import org.jfree.data.xy.XYDataItem;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;

/**
 *
 * @author Riccardo De Benedictis
 */
public class LessonController implements Initializable {

    @FXML
    private Label lesson_name_label;
    @FXML
    private TextField lesson_name;
    @FXML
    private Button play_button;
    @FXML
    private Button pause_button;
    @FXML
    private Button stop_button;
    @FXML
    private Label time_label;
    @FXML
    private TextField time;
    @FXML
    private StackPane lesson_chart_pane;
    @FXML
    private TableView<TokenRow> tokens_table_view;
    @FXML
    private TableColumn<TokenRow, Long> time_column;
    @FXML
    private TableColumn<TokenRow, Long> min_column;
    @FXML
    private TableColumn<TokenRow, Long> max_column;
    @FXML
    private TableColumn<TokenRow, String> id_column;
    @FXML
    private TableColumn<TokenRow, String> topics_column;
    @FXML
    private TableColumn<TokenRow, String> subject_column;
    private final ObjectProperty<TeachingLessonContext> l_ctx = new SimpleObjectProperty<>();
    private final TokenXYSeries tokens = new TokenXYSeries("Tokens");
    private final ValueMarker t_now = new ValueMarker(0);
    private final LongProperty t_now_property = new SimpleLongProperty(0);
    private final ChangeListener<Number> TIME_LISTENER = (ObservableValue<? extends Number> observable, Number oldValue, Number newValue) -> {
        time.setText(TIME_STRING_CONVERTER.toString(newValue.longValue()));
        final Timeline tl = new Timeline();
        final KeyValue kv = new KeyValue(t_now_property, newValue.longValue(), Interpolator.EASE_BOTH);
        final KeyFrame kf = new KeyFrame(Duration.millis(500), kv);
        tl.getKeyFrames().add(kf);
        tl.play();
    };
    private final ChangeListener<Lesson.LessonState> STATE_LISTENER = (ObservableValue<? extends Lesson.LessonState> observable, Lesson.LessonState oldValue, Lesson.LessonState newValue) -> {
        if (newValue != null) {
            switch (newValue) {
                case Running:
                    play_button.setDisable(true);
                    pause_button.setDisable(false);
                    stop_button.setDisable(false);
                    break;
                case Paused:
                    play_button.setDisable(false);
                    pause_button.setDisable(true);
                    stop_button.setDisable(false);
                    break;
                case Stopped:
                    play_button.setDisable(false);
                    pause_button.setDisable(true);
                    stop_button.setDisable(true);
                    break;
                default:
                    throw new AssertionError(newValue.name());
            }
        }
    };
    private static final StringConverter TIME_STRING_CONVERTER = new TimeStringConverter();
    private final ChangeListener<Number> TOKENS_TIME_CHANGE_LISTENER = (ObservableValue<? extends Number> observable, Number oldValue, Number newValue) -> {
        List<XYDataItem> items = new ArrayList<>(tokens.getItemCount());
        for (int i = 0; i < tokens.getItemCount(); i++) {
            if (((TokenXYDataItem) tokens.getDataItem(i)).t.timeProperty() == observable) {
                long x = ((TokenXYDataItem) tokens.getDataItem(i)).t.getTime();
                long y = 1;
                for (int j = 0; j < tokens.getItemCount(); j++) {
                    if (tokens.getDataItem(j).getXValue() == x) {
                        y++;
                    }
                }
                items.add(new TokenXYDataItem(x, y, ((TokenXYDataItem) tokens.getDataItem(i)).t));
            } else {
                items.add(tokens.getDataItem(i));
            }
        }
        tokens.clear();
        for (XYDataItem item : items) {
            tokens.add(item);
        }
    };
    private final ListChangeListener<TokenRow> TOKENS_CHANGE_LISTENER = (ListChangeListener.Change<? extends TokenRow> c) -> {
        while (c.next()) {
            c.getAddedSubList().forEach(tk_row -> {
                long x = tk_row.getTime();
                long y = 1;
                for (int i = 0; i < tokens.getItemCount(); i++) {
                    if (tokens.getDataItem(i).getXValue() == x) {
                        y++;
                    }
                }
                tokens.add(new TokenXYDataItem(x, y, tk_row));
                tk_row.timeProperty().addListener(TOKENS_TIME_CHANGE_LISTENER);
            });
            c.getRemoved().forEach(tk_row -> {
                tk_row.timeProperty().removeListener(TOKENS_TIME_CHANGE_LISTENER);
                for (int i = 0; i < tokens.getItemCount(); i++) {
                    if (((TokenXYDataItem) tokens.getDataItem(i)).t == tk_row) {
                        tokens.remove(i);
                        break;
                    }
                }
            });
        }
    };

    @Override
    public void initialize(URL location, ResourceBundle resources) {
        lesson_name_label.setText(lesson_name_label.getText() + ":");
        time_label.setText(time_label.getText() + ":");
        l_ctx.addListener((ObservableValue<? extends TeachingLessonContext> observable, TeachingLessonContext oldValue, TeachingLessonContext newValue) -> {
            if (oldValue != null) {
                oldValue.timeProperty().removeListener(TIME_LISTENER);
                oldValue.stateProperty().removeListener(STATE_LISTENER);
                oldValue.tokensProperty().removeListener(TOKENS_CHANGE_LISTENER);
            }
            tokens.clear();
            if (newValue != null) {
                lesson_name.setText(newValue.getLesson().name);
                STATE_LISTENER.changed(newValue.stateProperty(), oldValue != null ? oldValue.stateProperty().getValue() : null, newValue.stateProperty().getValue());
                newValue.stateProperty().addListener(STATE_LISTENER);
                TIME_LISTENER.changed(newValue.timeProperty(), oldValue != null ? oldValue.timeProperty().getValue() : null, newValue.timeProperty().getValue());
                newValue.timeProperty().addListener(TIME_LISTENER);
                newValue.tokensProperty().forEach(tk_row -> {
                    long x = tk_row.getTime();
                    long y = 1;
                    for (int i = 0; i < tokens.getItemCount(); i++) {
                        if (tokens.getDataItem(i).getXValue() == x) {
                            y++;
                        }
                    }
                    tokens.add(new TokenXYDataItem(x, y, tk_row));
                    tk_row.timeProperty().addListener(TOKENS_TIME_CHANGE_LISTENER);
                });
                newValue.tokensProperty().addListener(TOKENS_CHANGE_LISTENER);
                tokens_table_view.setItems(new SortedList<>(newValue.tokensProperty(), (TokenRow r0, TokenRow r1) -> Long.compare(r0.getTime(), r1.getTime())));
            } else {
                lesson_name.setText(null);
                play_button.setDisable(true);
                pause_button.setDisable(true);
                stop_button.setDisable(true);
                time.setText(null);
            }
        });

        play_button.setGraphic(new Glyph("FontAwesome", FontAwesome.Glyph.PLAY).color(Color.INDIGO));
        play_button.setOnAction((ActionEvent event) -> Context.getContext().play(l_ctx.get().getLesson()));
        pause_button.setGraphic(new Glyph("FontAwesome", FontAwesome.Glyph.PAUSE).color(Color.INDIGO));
        pause_button.setOnAction((ActionEvent event) -> Context.getContext().pause(l_ctx.get().getLesson()));
        stop_button.setGraphic(new Glyph("FontAwesome", FontAwesome.Glyph.STOP).color(Color.INDIGO));
        stop_button.setOnAction((ActionEvent event) -> Context.getContext().stop(l_ctx.get().getLesson()));

        XYSeriesCollection series_collection = new XYSeriesCollection();
        series_collection.addSeries(tokens);

        XYShapeRenderer renderer = new XYShapeRenderer();
        renderer.setSeriesPaint(0, java.awt.Color.blue);
        NumberAxis range_axis = new NumberAxis("");
        range_axis.setVisible(false);
        range_axis.setUpperMargin(0.4);
        NumberAxis domain_axis = new NumberAxis("");
        domain_axis.setNumberFormatOverride(new NumberFormat() {
            @Override
            public StringBuffer format(double number, StringBuffer toAppendTo, FieldPosition pos) {
                return format((long) number, toAppendTo, pos);
            }

            @Override
            public StringBuffer format(long number, StringBuffer toAppendTo, FieldPosition pos) {
                return toAppendTo.append(TIME_STRING_CONVERTER.toString(number));
            }

            @Override
            public Number parse(String source, ParsePosition parsePosition) {
                throw new UnsupportedOperationException("Not supported yet.");
            }
        });
        XYPlot plot = new XYPlot(series_collection, domain_axis, range_axis, renderer);
        plot.setShadowGenerator(new DefaultShadowGenerator(5, java.awt.Color.black, 1, 2, -45));
        plot.setNoDataMessage(Context.LANGUAGE.getString("NO_DATA"));
        plot.setRangeGridlinesVisible(false);
        t_now.setLabel(Context.LANGUAGE.getString("NOW"));
        t_now.setLabelAnchor(RectangleAnchor.TOP_LEFT);
        t_now.setLabelTextAnchor(TextAnchor.TOP_RIGHT);
        plot.addDomainMarker(t_now);

        JFreeChart chart = new JFreeChart(null, JFreeChart.DEFAULT_TITLE_FONT, plot, false);
        chart.setBackgroundPaint(new java.awt.Color(244, 244, 244));

        t_now_property.addListener((ObservableValue<? extends Number> observable, Number oldValue, Number newValue) -> t_now.setValue(newValue.longValue()));

        lesson_chart_pane.getChildren().add(new ChartViewer(chart));

        time_column.setCellValueFactory(new PropertyValueFactory<>("time"));
        time_column.setEditable(true);
        time_column.setCellFactory((TableColumn<TokenRow, Long> param) -> new TimeTextFieldTableCell());
        time_column.setOnEditCommit((TableColumn.CellEditEvent<TokenRow, Long> event) -> {
            Context.getContext().setTime(l_ctx.get().getLesson(), event.getRowValue(), event.getNewValue());
        });
        min_column.setCellValueFactory(new PropertyValueFactory<>("min"));
        min_column.setCellFactory((TableColumn<TokenRow, Long> param) -> new TimeTextFieldTableCell());
        max_column.setCellValueFactory(new PropertyValueFactory<>("max"));
        max_column.setCellFactory((TableColumn<TokenRow, Long> param) -> new TimeTextFieldTableCell());

        id_column.setCellValueFactory(new PropertyValueFactory<>("name"));
        id_column.setCellFactory((TableColumn<TokenRow, String> param) -> new TableCell<TokenRow, String>() {
            @Override
            protected void updateItem(String item, boolean empty) {
                super.updateItem(item, empty);
                if (empty) {
                    setText(null);
                    styleProperty().unbind();
                    setStyle("");
                } else {
                    setText(item);

                    TokenRow row = getTableView().getItems().get(getIndex());
                    styleProperty().bind(Bindings.createStringBinding(() -> {
                        if (row.getExecuted()) {
                            return "-fx-font-weight: bold;";
                        } else {
                            return "";
                        }
                    }, row.executedProperty()));
                }
            }
        });
        topics_column.setCellValueFactory(new PropertyValueFactory<>("name"));
        topics_column.setCellFactory((TableColumn<TokenRow, String> param) -> new TableCell<TokenRow, String>() {
            @Override
            protected void updateItem(String item, boolean empty) {
                super.updateItem(item, empty);
                if (empty) {
                    setText(null);
                    styleProperty().unbind();
                    setStyle("");
                } else {
                    TokenRow row = getTableView().getItems().get(getIndex());
                    setText(l_ctx.get().getModel().stimuli.get(row.getName()).topics.stream().collect(Collectors.joining(", ")));

                    styleProperty().bind(Bindings.createStringBinding(() -> {
                        if (row.getExecuted()) {
                            return "-fx-font-weight: bold;";
                        } else {
                            return "";
                        }
                    }, row.executedProperty()));
                }
            }
        });
        subject_column.setCellValueFactory(new PropertyValueFactory<>("name"));
        subject_column.setCellFactory((TableColumn<TokenRow, String> param) -> new TableCell<TokenRow, String>() {
            @Override
            protected void updateItem(String item, boolean empty) {
                super.updateItem(item, empty);
                if (empty) {
                    setText(null);
                    styleProperty().unbind();
                    setStyle("");
                } else {
                    TokenRow row = getTableView().getItems().get(getIndex());
                    LessonModel.StimulusTemplate st = l_ctx.get().getModel().stimuli.get(row.getName());
                    if (st instanceof LessonModel.StimulusTemplate.TextStimulusTemplate) {
                        setText(((LessonModel.StimulusTemplate.TextStimulusTemplate) st).content);
                    } else if (st instanceof LessonModel.StimulusTemplate.URLStimulusTemplate) {
                        setText(((LessonModel.StimulusTemplate.URLStimulusTemplate) st).content);
                    } else if (st instanceof LessonModel.StimulusTemplate.QuestionStimulusTemplate) {
                        setText(((LessonModel.StimulusTemplate.QuestionStimulusTemplate) st).question);
                    }

                    styleProperty().bind(Bindings.createStringBinding(() -> {
                        if (row.getExecuted()) {
                            return "-fx-font-weight: bold;";
                        } else {
                            return "";
                        }
                    }, row.executedProperty()));
                }
            }
        });

        tokens_table_view.setRowFactory((TableView<TokenRow> param) -> {
            TableRow<TokenRow> row = new TableRow<>();
            row.itemProperty().addListener((ObservableValue<? extends TokenRow> observable, TokenRow oldValue, TokenRow newValue) -> {
                if (newValue == null) {
                    row.setContextMenu(null);
                } else {
                    row.setContextMenu(new NavigateContextMenu(row));
                }
            });
            return row;
        });
    }

    public ObjectProperty<TeachingLessonContext> teachingLessonContextProperty() {
        return l_ctx;
    }

    private class TokenXYSeries extends XYSeries {

        TokenXYSeries(Comparable key) {
            super(key);
        }

        public void add(double x, double y, TokenRow t) {
            super.add(new TokenXYDataItem(x, y, t));
        }
    }

    private class TokenXYDataItem extends XYDataItem {

        private final TokenRow t;

        TokenXYDataItem(double x, double y, TokenRow t) {
            super(x, y);
            this.t = t;
        }
    }

    private static class TimeTextFieldTableCell extends TextFieldTableCell<TokenRow, Long> {

        public TimeTextFieldTableCell() {
            super(TIME_STRING_CONVERTER);
        }

        @Override
        public void updateItem(Long item, boolean empty) {
            super.updateItem(item, empty);
            if (empty) {
                setText(null);
                editableProperty().unbind();
                styleProperty().unbind();
                setStyle("");
            } else {
                setText(TIME_STRING_CONVERTER.toString(item));

                TokenRow row = getTableView().getItems().get(getIndex());
                editableProperty().bind(row.executedProperty().not());
                styleProperty().bind(Bindings.createStringBinding(() -> {
                    if (row.getExecuted()) {
                        return "-fx-font-weight: bold;";
                    } else {
                        return "";
                    }
                }, row.executedProperty()));
            }
        }
    }

    private class NavigateContextMenu extends ContextMenu {

        private final MenuItem go_to = new MenuItem(Context.LANGUAGE.getString("GO_TO"), new Glyph("FontAwesome", FontAwesome.Glyph.SHARE));
        private final MenuItem edit = new MenuItem(Context.LANGUAGE.getString("EDIT_TIME"), new Glyph("FontAwesome", FontAwesome.Glyph.EDIT));

        public NavigateContextMenu(TableRow<TokenRow> row) {
            go_to.setOnAction((ActionEvent event) -> Context.getContext().goTo(l_ctx.get().getLesson(), row.getItem().getTime()));
            edit.setOnAction((ActionEvent event) -> tokens_table_view.edit(row.getIndex(), tokens_table_view.getColumns().get(0)));
            edit.disableProperty().bind(row.getItem().executedProperty());
            getItems().addAll(go_to, new SeparatorMenuItem(), edit);
        }
    }
}
