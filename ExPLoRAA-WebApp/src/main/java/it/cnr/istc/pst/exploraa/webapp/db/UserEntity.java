/*
 * Copyright (C) 2018 Riccardo De Benedictis
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package it.cnr.istc.pst.exploraa.webapp.db;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.OneToMany;

/**
 *
 * @author Riccardo De Benedictis
 */
@Entity
public class UserEntity implements Serializable {

    private static final long serialVersionUID = 1L;
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private Long id;
    @Column(unique = true)
    private String email;
    private String password;
    private String first_name;
    private String last_name;
    @OneToMany
    private final Collection<LessonModelEntity> models = new ArrayList<>();
    @OneToMany(mappedBy = "student", cascade = CascadeType.ALL, orphanRemoval = true)
    private final Collection<FollowEntity> follows = new ArrayList<>();
    @OneToMany(mappedBy = "teacher", cascade = CascadeType.ALL, orphanRemoval = true)
    private final Collection<TeachEntity> teachs = new ArrayList<>();

    public Long getId() {
        return id;
    }

    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
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

    public Collection<LessonModelEntity> getModels() {
        return Collections.unmodifiableCollection(models);
    }

    public void addModel(LessonModelEntity model) {
        models.add(model);
    }

    public void removeModel(LessonModelEntity model) {
        models.remove(model);
    }

    public Collection<FollowEntity> getFollowedLessons() {
        return Collections.unmodifiableCollection(follows);
    }

    public void addFollowedLesson(FollowEntity model) {
        follows.add(model);
    }

    public void removeFollowedLesson(FollowEntity model) {
        follows.remove(model);
    }

    public Collection<TeachEntity> getTeachedLessons() {
        return Collections.unmodifiableCollection(teachs);
    }

    public void addTeachedLesson(TeachEntity model) {
        teachs.add(model);
    }

    public void removeTeachedLesson(TeachEntity model) {
        teachs.remove(model);
    }
}
