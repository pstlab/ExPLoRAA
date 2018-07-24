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

import it.cnr.istc.pst.exploraa.api.User;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.SimpleBooleanProperty;

/**
 *
 * @author Riccardo De Benedictis
 */
public class TeacherContext {

    private final User teacher;
    private final BooleanProperty on_line;

    TeacherContext(User teacher) {
        this.teacher = teacher;
        this.on_line = new SimpleBooleanProperty(teacher.online);
    }

    public User getTeacher() {
        return teacher;
    }

    public boolean isOnline() {
        return on_line.get();
    }

    public BooleanProperty onlineProperty() {
        return on_line;
    }
}
