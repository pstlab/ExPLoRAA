package it.cnr.istc.pst.exploraa.db;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.ManyToMany;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;

@Entity
public class LessonEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private Long id;
    private String name;
    @ManyToOne(cascade = CascadeType.ALL)
    private LessonModelEntity model;
    @ManyToOne(cascade = CascadeType.ALL)
    private UserEntity teacher;
    @ManyToMany(cascade = CascadeType.ALL)
    private final Collection<UserEntity> followed_by = new ArrayList<>();
    @OneToMany
    private final Collection<StimulusEntity> goals = new ArrayList<>();

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

    public UserEntity getTeacher() {
        return teacher;
    }

    public void setTeacher(UserEntity teacher) {
        this.teacher = teacher;
    }

    public Collection<UserEntity> getStudents() {
        return Collections.unmodifiableCollection(followed_by);
    }

    public void addStudent(UserEntity student) {
        followed_by.add(student);
    }

    public void removeStudent(UserEntity student) {
        followed_by.remove(student);
    }

    public Collection<StimulusEntity> getGoals() {
        return Collections.unmodifiableCollection(goals);
    }

    public void addGoal(StimulusEntity goal) {
        goals.add(goal);
    }

    public void removeGoal(StimulusEntity goal) {
        goals.remove(goal);
    }
}
