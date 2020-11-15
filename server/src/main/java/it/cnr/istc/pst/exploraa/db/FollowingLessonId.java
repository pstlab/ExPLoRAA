package it.cnr.istc.pst.exploraa.db;

import javax.persistence.Embeddable;

@Embeddable
public class FollowingLessonId {

    private Long student_id;
    private Long lesson_id;

    public FollowingLessonId() {
    }

    public FollowingLessonId(Long student_id, Long lesson_id) {
        this.student_id = student_id;
        this.lesson_id = lesson_id;
    }

    /**
     * @return the student_id
     */
    public Long getStudentId() {
        return student_id;
    }

    /**
     * @return the lesson_id
     */
    public Long getLessonId() {
        return lesson_id;
    }
}