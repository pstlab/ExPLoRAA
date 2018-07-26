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
package it.cnr.istc.pst.exploraa.webapp.db;

import java.io.Serializable;
import javax.persistence.CascadeType;
import javax.persistence.EmbeddedId;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.MapsId;
import javax.persistence.OneToOne;

/**
 *
 * @author Riccardo De Benedictis
 */
@Entity
public class TeachEntity implements Serializable {

    private static final long serialVersionUID = 1L;
    @EmbeddedId
    private TeachId id;
    @ManyToOne(fetch = FetchType.LAZY)
    @MapsId("teacher_id")
    private UserEntity teacher;
    @OneToOne(mappedBy = "teached_by", cascade = CascadeType.ALL)
    private LessonEntity lesson;

    public TeachEntity() {
    }

    public TeachEntity(UserEntity teacher, LessonEntity lesson) {
        this.id = new TeachId(teacher.getId(), lesson.getId());
        this.teacher = teacher;
        this.lesson = lesson;
    }

    public UserEntity getTeacher() {
        return teacher;
    }

    public void setTeacher(UserEntity teacher) {
        this.teacher = teacher;
    }

    public LessonEntity getLesson() {
        return lesson;
    }

    public void setLesson(LessonEntity lesson) {
        this.lesson = lesson;
    }
}
