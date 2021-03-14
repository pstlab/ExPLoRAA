package it.cnr.istc.pst.exploraa.db;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.ElementCollection;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Index;
import javax.persistence.ManyToMany;
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
    private final Set<String> roles = new HashSet<>();
    @ElementCollection
    private final Set<String> interests = new HashSet<>();
    @OneToMany
    private final Collection<LessonModelEntity> models = new ArrayList<>();
    @ManyToMany(mappedBy = "students", cascade = CascadeType.ALL)
    private final Collection<UserEntity> teachers = new ArrayList<>();
    @ManyToMany(cascade = CascadeType.ALL)
    private final Collection<UserEntity> students = new ArrayList<>();
    @ManyToMany(mappedBy = "followed_by", cascade = CascadeType.ALL)
    private final Collection<LessonEntity> following_lessons = new ArrayList<>();
    @OneToMany(mappedBy = "teacher", cascade = CascadeType.ALL, orphanRemoval = true)
    private final Collection<LessonEntity> teaching_lessons = new ArrayList<>();

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
    public Set<String> getRoles() {
        return Collections.unmodifiableSet(roles);
    }

    public void addRole(String role) {
        roles.add(role);
    }

    public void removeRole(String role) {
        roles.remove(role);
    }

    /**
     * @return the interests.
     */
    public Set<String> getInterests() {
        return Collections.unmodifiableSet(interests);
    }

    public void addInterest(String interest) {
        roles.add(interest);
    }

    public void removeInterest(String interest) {
        roles.remove(interest);
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

    public Collection<UserEntity> getStudents() {
        return Collections.unmodifiableCollection(students);
    }

    public void addStudent(UserEntity student) {
        students.add(student);
    }

    public void removeStudent(UserEntity student) {
        students.remove(student);
    }

    public Collection<UserEntity> getTeachers() {
        return Collections.unmodifiableCollection(teachers);
    }

    public void addTeacher(UserEntity teacher) {
        teachers.add(teacher);
    }

    public void removeTeacher(UserEntity teacher) {
        teachers.remove(teacher);
    }

    public Collection<LessonEntity> getFollowingLessons() {
        return Collections.unmodifiableCollection(following_lessons);
    }

    public void addFollowingLesson(LessonEntity model) {
        following_lessons.add(model);
    }

    public void removeFollowingLesson(LessonEntity model) {
        following_lessons.remove(model);
    }

    public Collection<LessonEntity> getTeachingLessons() {
        return Collections.unmodifiableCollection(teaching_lessons);
    }

    public void addTeachingLesson(LessonEntity model) {
        teaching_lessons.add(model);
    }

    public void removeTeachingLesson(LessonEntity model) {
        teaching_lessons.remove(model);
    }
}
