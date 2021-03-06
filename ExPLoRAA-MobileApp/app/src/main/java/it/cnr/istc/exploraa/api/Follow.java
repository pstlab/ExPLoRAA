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
package it.cnr.istc.exploraa.api;

import java.util.Set;

/**
 * @author Riccardo De Benedictis
 */
public class Follow {

    public User user;
    public Lesson lesson;
    public Set<String> interests;

    public Follow() {
    }

    public Follow(User user, Lesson lesson, Set<String> interests) {
        this.user = user;
        this.lesson = lesson;
        this.interests = interests;
    }
}
