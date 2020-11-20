package it.cnr.istc.pst.exploraa.db;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

import javax.persistence.CollectionTable;
import javax.persistence.ElementCollection;
import javax.persistence.EmbeddedId;
import javax.persistence.Entity;
import javax.persistence.ManyToOne;
import javax.persistence.MapsId;

@Entity
public class FollowingLessonEntity {

    @EmbeddedId
    private FollowingLessonId id;
    @ManyToOne
    @MapsId("student_id")
    private UserEntity student;
    @ManyToOne
    @MapsId("lesson_id")
    private LessonEntity lesson;
    @ElementCollection
    @CollectionTable(name = "Interests")
    private final Collection<String> interests = new ArrayList<>();

    public FollowingLessonEntity() {
    }

    public FollowingLessonEntity(UserEntity student, LessonEntity lesson) {
        this.id = new FollowingLessonId(student.getId(), lesson.getId());
        this.student = student;
        this.lesson = lesson;
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

    /**
     * @return the interests
     */
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