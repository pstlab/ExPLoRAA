/*
 * Copyright (C) 2018 Riccardo De Benedictis
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
package it.cnr.istc.pst.exploraa.api;

import java.util.Collection;
import java.util.Map;

/**
 *
 * @author Riccardo De Benedictis
 */
public class Lesson {

    public long id;
    public Teach teacher;
    public String name;
    public LessonState state;
    public long time;
    public LessonModel model;
    public Map<Long, Follow> students;
    public Collection<Message.Stimulus> stimuli;
    public Collection<Message.Token> tokens;

    public Lesson() {
    }

    public Lesson(long id, Teach teacher, String name, LessonState state, long time, LessonModel model, Map<Long, Follow> students, Collection<Message.Stimulus> stimuli, Collection<Message.Token> tokens) {
        this.id = id;
        this.teacher = teacher;
        this.name = name;
        this.state = state;
        this.time = time;
        this.model = model;
        this.students = students;
        this.stimuli = stimuli;
        this.tokens = tokens;
    }

    public enum LessonState {
        Running, Paused, Stopped
    }
}
