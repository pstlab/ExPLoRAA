package it.cnr.istc.pst.exploraa.db;

import javax.persistence.EmbeddedId;
import javax.persistence.Entity;
import javax.persistence.ManyToOne;
import javax.persistence.MapsId;

@Entity
public class FollowingEntity {

    @EmbeddedId
    private FollowingId id;
    @ManyToOne
    @MapsId("student_id")
    private UserEntity student;
    @ManyToOne
    @MapsId("lesson_id")
    private UserEntity teacher;

    public FollowingEntity() {
    }

    public FollowingEntity(UserEntity student, UserEntity teacher) {
        this.id = new FollowingId(student.getId(), teacher.getId());
        this.student = student;
        this.teacher = teacher;
    }

    /**
     * @return the student
     */
    public UserEntity getStudent() {
        return student;
    }

    /**
     * @param student the student to set
     */
    public void setStudent(UserEntity student) {
        this.student = student;
    }

    /**
     * @return the teacher
     */
    public UserEntity getTeacher() {
        return teacher;
    }

    /**
     * @param teacher the teacher to set
     */
    public void setTeacher(UserEntity teacher) {
        this.teacher = teacher;
    }
}