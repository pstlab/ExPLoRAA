/*
 * Copyright (C) 2018 Your Organisation
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
package it.cnr.istc.pst.exploraa.desktopapp;

import it.cnr.istc.pst.exploraa.api.ExPLoRAA;
import it.cnr.istc.pst.exploraa.api.Lesson;
import it.cnr.istc.pst.exploraa.api.User;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Properties;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.ClientBuilder;
import javax.ws.rs.client.Entity;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.Form;
import javax.ws.rs.core.GenericType;
import javax.ws.rs.core.MediaType;

/**
 *
 * @author Riccardo De Benedictis
 */
class ExPLoRAAResource implements ExPLoRAA {

    private final Client client = ClientBuilder.newClient();
    private final WebTarget target;

    ExPLoRAAResource(final Properties properties) {
        this.target = client.target("http://" + properties.getProperty("host") + ":" + properties.getProperty("service-port")).path("ExPLoRAA").path("resources");
    }

    @Override
    public User login(String email, String password) {
        Form login_form = new Form();
        login_form.param("email", email);
        login_form.param("password", password);
        return target.path("login").request(MediaType.APPLICATION_JSON).post(Entity.form(login_form), User.class);
    }

    @Override
    public User new_user(String email, String password, String first_name, String last_name) {
        Form new_user_form = new Form();
        new_user_form.param("email", email);
        new_user_form.param("password", password);
        new_user_form.param("first_name", first_name);
        new_user_form.param("last_name", last_name);
        return target.path("new_user").request(MediaType.APPLICATION_JSON).post(Entity.form(new_user_form), User.class);
    }

    @Override
    public void delete_user(long id) {
        target.path("user").path(Long.toString(id)).request().delete();
    }

    @Override
    public Collection<User> get_users() {
        return target.path("users").request(MediaType.APPLICATION_JSON).get(new GenericType<ArrayList<User>>() {
        });
    }

    @Override
    public Lesson new_lesson(long teacher_id, String name, String model) {
        Form new_lesson_form = new Form();
        new_lesson_form.param("teacher_id", Long.toString(teacher_id));
        new_lesson_form.param("name", name);
        new_lesson_form.param("model", model);
        return target.path("new_lesson_by_model").request(MediaType.APPLICATION_JSON).post(Entity.form(new_lesson_form), Lesson.class);
    }

    @Override
    public Lesson new_lesson(long teacher_id, String name, long model_id) {
        Form new_lesson_form = new Form();
        new_lesson_form.param("teacher_id", Long.toString(teacher_id));
        new_lesson_form.param("name", name);
        new_lesson_form.param("model_id", Long.toString(model_id));
        return target.path("new_lesson_by_model_id").request(MediaType.APPLICATION_JSON).post(Entity.form(new_lesson_form), Lesson.class);
    }

    @Override
    public void delete_lesson(long id) {
        target.path("lesson").path(Long.toString(id)).request().delete();
    }

    @Override
    public Collection<Lesson> get_lessons() {
        return target.path("lessons").request(MediaType.APPLICATION_JSON).get(new GenericType<ArrayList<Lesson>>() {
        });
    }

    @Override
    public void follow(long user_id, long lesson_id, String interests) {
        Form follow_form = new Form();
        follow_form.param("user_id", Long.toString(user_id));
        follow_form.param("lesson_id", Long.toString(lesson_id));
        follow_form.param("interests", interests);
        target.path("follow").request(MediaType.APPLICATION_JSON).post(Entity.form(follow_form));
    }

    @Override
    public void unfollow(long user_id, long lesson_id) {
        Form follow_form = new Form();
        follow_form.param("user_id", Long.toString(user_id));
        follow_form.param("lesson_id", Long.toString(lesson_id));
        target.path("unfollow").request(MediaType.APPLICATION_JSON).post(Entity.form(follow_form));
    }

    @Override
    public void solve(long id) {
        Form solve_form = new Form();
        solve_form.param("id", Long.toString(id));
        target.path("solve").path(Long.toString(id)).request().post(Entity.form(solve_form));
    }

    @Override
    public void play(long id) {
        Form form = new Form();
        form.param("id", Long.toString(id));
        target.path("play").request().put(Entity.form(form));
    }

    @Override
    public void pause(long id) {
        Form form = new Form();
        form.param("id", Long.toString(id));
        target.path("pause").request().put(Entity.form(form));
    }

    @Override
    public void stop(long id) {
        Form form = new Form();
        form.param("id", Long.toString(id));
        target.path("stop").request().put(Entity.form(form));
    }

    @Override
    public void go_to(long id, long time) {
        Form form = new Form();
        form.param("id", Long.toString(id));
        form.param("time", Long.toString(time));
        target.path("go_to").request().put(Entity.form(form));
    }
}
