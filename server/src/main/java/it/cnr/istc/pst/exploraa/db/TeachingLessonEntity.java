package it.cnr.istc.pst.exploraa.db;

import javax.persistence.EmbeddedId;
import javax.persistence.Entity;
import javax.persistence.ManyToOne;
import javax.persistence.MapsId;

@Entity
public class TeachingLessonEntity {

    @EmbeddedId
    private TeachingLessonId id;
    @ManyToOne
    @MapsId("teacher_id")
    private UserEntity teacher;
    @ManyToOne
    @MapsId("lesson_id")
    private LessonEntity lesson;

    public TeachingLessonEntity() {
    }

    public TeachingLessonEntity(UserEntity teacher, LessonEntity lesson) {
        this.id = new TeachingLessonId(teacher.getId(), lesson.getId());
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