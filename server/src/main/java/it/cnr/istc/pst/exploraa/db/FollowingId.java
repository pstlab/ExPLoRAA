package it.cnr.istc.pst.exploraa.db;

import javax.persistence.Embeddable;

@Embeddable
public class FollowingId {

    private Long student_id;
    private Long teacher_id;

    public FollowingId() {
    }

    public FollowingId(Long student_id, Long teacher_id) {
        this.student_id = student_id;
        this.teacher_id = teacher_id;
    }

    /**
     * @return the student_id
     */
    public Long getStudentId() {
        return student_id;
    }

    /**
     * @return the teacher_id
     */
    public Long getTeacherId() {
        return teacher_id;
    }
}