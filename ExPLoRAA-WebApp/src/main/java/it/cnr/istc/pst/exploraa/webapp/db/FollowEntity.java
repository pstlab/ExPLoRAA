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
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import javax.persistence.ElementCollection;
import javax.persistence.EmbeddedId;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.MapsId;

/**
 *
 * @author Riccardo De Benedictis
 */
@Entity
public class FollowEntity implements Serializable {

    private static final long serialVersionUID = 1L;
    @EmbeddedId
    private FollowId id;
    @ManyToOne(fetch = FetchType.LAZY)
    @MapsId("user_id")
    private UserEntity student;
    @ManyToOne(fetch = FetchType.LAZY)
    @MapsId("lesson_id")
    private LessonEntity lesson;
    @ElementCollection
    private final Collection<String> interests = new ArrayList<>();

    public FollowEntity() {
    }

    public FollowEntity(UserEntity student, LessonEntity lesson) {
        this.id = new FollowId(student.getId(), lesson.getId());
        this.student = student;
        this.lesson = lesson;
    }

    public UserEntity getStudent() {
        return student;
    }

    public void setStudent(UserEntity student) {
        this.student = student;
    }

    public LessonEntity getLesson() {
        return lesson;
    }

    public void setLesson(LessonEntity lesson) {
        this.lesson = lesson;
    }

    public Collection<String> getInterests() {
        return Collections.unmodifiableCollection(interests);
    }

    public void addInterest(String interest) {
        interests.add(interest);
    }

    public void removeInterest(String interest) {
        interests.remove(interest);
    }
}
