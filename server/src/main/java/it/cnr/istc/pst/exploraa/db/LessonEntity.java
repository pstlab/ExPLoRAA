package it.cnr.istc.pst.exploraa.db;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

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
    @ManyToOne
    private ModelEntity model;
    @ManyToOne
    private UserEntity teacher;
    @ManyToMany
    private final Collection<UserEntity> followed_by = new ArrayList<>();
    @OneToMany
    private final Collection<RuleEntity> goals = new ArrayList<>();

    public Long getId() {
        return id;
    }

    public String getName() {
        return name;
    }

    public void setName(final String name) {
        this.name = name;
    }

    /**
     * @return the model
     */
    public ModelEntity getModel() {
        return model;
    }

    /**
     * @param model the model to set
     */
    public void setModel(final ModelEntity model) {
        this.model = model;
    }

    public UserEntity getTeacher() {
        return teacher;
    }

    public void setTeacher(final UserEntity teacher) {
        this.teacher = teacher;
    }

    public Collection<UserEntity> getStudents() {
        return Collections.unmodifiableCollection(followed_by);
    }

    public void addStudent(final UserEntity student) {
        followed_by.add(student);
    }

    public void removeStudent(final UserEntity student) {
        followed_by.remove(student);
    }

    public Collection<RuleEntity> getGoals() {
        return Collections.unmodifiableCollection(goals);
    }

    public void addGoal(final RuleEntity goal) {
        goals.add(goal);
    }

    public void removeGoal(final RuleEntity goal) {
        goals.remove(goal);
    }
}
