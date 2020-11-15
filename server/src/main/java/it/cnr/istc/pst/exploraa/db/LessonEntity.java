package it.cnr.istc.pst.exploraa.db;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;

@Entity
public class LessonEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private Long id;
    private String name;
    @ManyToOne
    private LessonModelEntity model;
    @OneToOne(cascade = CascadeType.ALL, orphanRemoval = true)
    private TeachingLessonEntity teached_by;
    @OneToMany(mappedBy = "lesson", cascade = CascadeType.ALL, orphanRemoval = true)
    private final Collection<FollowingLessonEntity> followed_by = new ArrayList<>();

    public Long getId() {
        return id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    /**
     * @return the model
     */
    public LessonModelEntity getModel() {
        return model;
    }

    /**
     * @param model the model to set
     */
    public void setModel(LessonModelEntity model) {
        this.model = model;
    }

    public TeachingLessonEntity getTeacher() {
        return teached_by;
    }

    public void setTeacher(TeachingLessonEntity teached_by) {
        this.teached_by = teached_by;
    }

    public Collection<FollowingLessonEntity> getStudents() {
        return Collections.unmodifiableCollection(followed_by);
    }

    public void addStudent(FollowingLessonEntity student) {
        followed_by.add(student);
    }

    public void removeStudent(FollowingLessonEntity student) {
        followed_by.remove(student);
    }
}
