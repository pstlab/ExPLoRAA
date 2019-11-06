package it.cnr.istc.pst.exploraa.server.db;

import javax.persistence.EmbeddedId;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.MapsId;

/**
 * FollowEntity
 */
@Entity
public class TeachEntity {

    @EmbeddedId
    private TeachId id;
    @ManyToOne(fetch = FetchType.LAZY)
    @MapsId("teacher_id")
    private UserEntity teacher;
    @ManyToOne(fetch = FetchType.LAZY)
    @MapsId("lesson_id")
    private LessonEntity lesson;

    public TeachEntity() {
    }

    public TeachEntity(UserEntity teacher, LessonEntity lesson) {
        this.id = new TeachId(teacher.getId(), lesson.getId());
        this.teacher = teacher;
        this.lesson = lesson;
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

    /**
     * @return the lesson
     */
    public LessonEntity getLesson() {
        return lesson;
    }

    /**
     * @param lesson the lesson to set
     */
    public void setLesson(LessonEntity lesson) {
        this.lesson = lesson;
    }
}