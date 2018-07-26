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
import javax.persistence.Embeddable;

/**
 *
 * @author Riccardo De Benedictis
 */
@Embeddable
public class FollowId implements Serializable {

    private Long student_id;
    private Long lesson_id;

    public FollowId() {
    }

    public FollowId(Long student_id, Long lesson_id) {
        this.student_id = student_id;
        this.lesson_id = lesson_id;
    }

    public Long getStudentId() {
        return student_id;
    }

    public Long getLessonId() {
        return lesson_id;
    }
}
