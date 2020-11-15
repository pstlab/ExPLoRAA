package it.cnr.istc.pst.exploraa.db;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.ElementCollection;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Index;
import javax.persistence.OneToMany;
import javax.persistence.Table;

@Entity
@Table(indexes = { @Index(name = "email_index", columnList = "email", unique = true) })
public class UserEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private Long id;
    @Column(nullable = false)
    private String email;
    @Column(nullable = false, length = 684)
    private String salt;
    @Column(nullable = false)
    private String password;
    private String first_name;
    private String last_name;
    @ElementCollection
    private final Collection<String> roles = new ArrayList<>();
    @OneToMany
    private final Collection<LessonModelEntity> models = new ArrayList<>();
    @OneToMany(mappedBy = "student", cascade = CascadeType.ALL, orphanRemoval = true)
    private final Collection<FollowingEntity> teachers = new ArrayList<>();
    @OneToMany(mappedBy = "teacher", cascade = CascadeType.ALL, orphanRemoval = true)
    private final Collection<FollowingEntity> students = new ArrayList<>();
    @OneToMany(mappedBy = "student", cascade = CascadeType.ALL, orphanRemoval = true)
    private final Collection<FollowingLessonEntity> following_lessons = new ArrayList<>();
    @OneToMany(mappedBy = "teacher", cascade = CascadeType.ALL, orphanRemoval = true)
    private final Collection<TeachingLessonEntity> teaching_lessons = new ArrayList<>();

    public Long getId() {
        return id;
    }

    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    public String getSalt() {
        return salt;
    }

    public void setSalt(String salt) {
        this.salt = salt;
    }

    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    public String getFirstName() {
        return first_name;
    }

    public void setFirstName(String first_name) {
        this.first_name = first_name;
    }

    public String getLastName() {
        return last_name;
    }

    public void setLastName(String last_name) {
        this.last_name = last_name;
    }

    /**
     * @return the roles.
     */
    public Collection<String> getRoles() {
        return Collections.unmodifiableCollection(roles);
    }

    public void addRole(String role) {
        roles.add(role);
    }

    public void removeRole(String role) {
        roles.remove(role);
    }

    public Collection<LessonModelEntity> getModels() {
        return Collections.unmodifiableCollection(models);
    }

    public void addModel(LessonModelEntity model) {
        models.add(model);
    }

    public void removeModel(LessonModelEntity model) {
        models.remove(model);
    }

    public Collection<FollowingEntity> getStudents() {
        return Collections.unmodifiableCollection(students);
    }

    public void addStudent(FollowingEntity student) {
        students.add(student);
    }

    public void removeStudent(FollowingEntity student) {
        students.remove(student);
    }

    public Collection<FollowingEntity> getTeachers() {
        return Collections.unmodifiableCollection(teachers);
    }

    public void addTeacher(FollowingEntity teacher) {
        teachers.add(teacher);
    }

    public void removeTeacher(FollowingEntity teacher) {
        teachers.remove(teacher);
    }

    public Collection<FollowingLessonEntity> getFollowingLessons() {
        return Collections.unmodifiableCollection(following_lessons);
    }

    public void addFollowingLesson(FollowingLessonEntity model) {
        following_lessons.add(model);
    }

    public void removeFollowingLesson(FollowingLessonEntity model) {
        following_lessons.remove(model);
    }

    public Collection<TeachingLessonEntity> getTeachingLessons() {
        return Collections.unmodifiableCollection(teaching_lessons);
    }

    public void addTeachingLesson(TeachingLessonEntity model) {
        teaching_lessons.add(model);
    }

    public void removeTeachingLesson(TeachingLessonEntity model) {
        teaching_lessons.remove(model);
    }
}
