package it.cnr.istc.pst.exploraa.server.db;

import javax.persistence.Embeddable;

/**
 * FollowId
 */
@Embeddable
public class TeachId {

    private Long teacher_id;
    private Long lesson_id;

    public TeachId() {
    }

    public TeachId(Long teacher_id, Long lesson_id) {
        this.teacher_id = teacher_id;
        this.lesson_id = lesson_id;
    }

    /**
     * @return the teacher_id
     */
    public Long getTeacherId() {
        return teacher_id;
    }

    /**
     * @return the lesson_id
     */
    public Long getLessonId() {
        return lesson_id;
    }
}