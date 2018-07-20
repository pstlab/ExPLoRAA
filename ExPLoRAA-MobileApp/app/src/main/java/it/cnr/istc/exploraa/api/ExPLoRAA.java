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

import java.util.Collection;

import retrofit2.http.GET;
import retrofit2.http.POST;

/**
 * @author Riccardo De Benedictis
 */
public interface ExPLoRAA {

    @POST("ExPLoRA/resources/login")
    User login(String email, String password);

    @POST("ExPLoRA/resources/new_user")
    User new_user(String email, String password, String first_name, String last_name);

    @GET("ExPLoRA/resources/users")
    Collection<User> getUsers();
}
