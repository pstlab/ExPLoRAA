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
package it.cnr.istc.pst.exploraa.api;

import java.util.Collection;

/**
 *
 * @author Riccardo De Benedictis
 */
public interface ExPLoRAA {

    public User login(String email, String password);

    public User new_user(String email, String password, String first_name, String last_name);

    public void delete_user(long id);

    public Collection<User> get_users();

    public Lesson new_lesson(long teacher_id, String name, String model);

    public Lesson new_lesson(long teacher_id, String name, long id);

    public void delete_lesson(long id);

    public Collection<Lesson> get_lessons();

    public void solve(long id);

    public void play(long id);

    public void pause(long id);

    public void stop(long id);

    public void go_to(long id, long time);
}
