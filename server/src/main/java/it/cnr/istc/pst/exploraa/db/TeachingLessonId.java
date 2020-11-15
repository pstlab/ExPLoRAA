package it.cnr.istc.pst.exploraa.db;

import javax.persistence.Embeddable;

@Embeddable
public class TeachingLessonId {

    private Long teacher_id;
    private Long lesson_id;

    public TeachingLessonId() {
    }

    public TeachingLessonId(Long teacher_id, Long lesson_id) {
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