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

import retrofit2.Call;
import retrofit2.http.DELETE;
import retrofit2.http.Field;
import retrofit2.http.FormUrlEncoded;
import retrofit2.http.GET;
import retrofit2.http.POST;
import retrofit2.http.PUT;
import retrofit2.http.Path;

/**
 * @author Riccardo De Benedictis
 */
public interface ExPLoRAA {

    @FormUrlEncoded
    @POST("login")
    Call<User> login(@Field("email") String email, @Field("password") String password);

    @FormUrlEncoded
    @POST("new_user")
    Call<User> new_user(@Field("email") String email, @Field("password") String password, @Field("first_name") String first_name, @Field("last_name") String last_name);

    @DELETE("user/{id}")
    void delete_user(@Path("id") long id);

    @GET("users")
    Call<Collection<User>> get_users();

    @FormUrlEncoded
    @POST("new_lesson_by_model")
    Call<Lesson> new_lesson(@Field("teacher_id") long teacher_id, @Field("name") String name, @Field("model") String model);

    @FormUrlEncoded
    @POST("new_lesson_by_model_id")
    Call<Lesson> new_lesson(@Field("teacher_id") long teacher_id, @Field("name") String name, @Field("model_id") long model_id);

    @DELETE("lesson/{id}")
    Call<Void> delete_lesson(@Path("id") long id);

    @GET("lessons")
    Call<Collection<Lesson>> get_lessons();

    @FormUrlEncoded
    @PUT("login")
    Call<Void> follow(@Field("user_id") long user_id, @Field("lesson_id") long lesson_id, @Field("interests") String interests);

    @FormUrlEncoded
    @PUT("login")
    Call<Void> unfollow(@Field("user_id") long user_id, @Field("lesson_id") long lesson_id);

    @FormUrlEncoded
    @PUT("login")
    Call<Void> answer_question(@Field("lesson_id") long lesson_id, @Field("question_id") int question_id, @Field("answer_id") int answer_id);

    @FormUrlEncoded
    @PUT("login")
    Call<Void> solve(@Field("id") long id);

    @FormUrlEncoded
    @PUT("login")
    Call<Void> set_time(@Field("lesson_id") long lesson_id, @Field("token_id") int token_id, @Field("time") long time);

    @FormUrlEncoded
    @PUT("login")
    Call<Void> play(@Field("id") long id);

    @FormUrlEncoded
    @PUT("login")
    Call<Void> pause(@Field("id") long id);

    @FormUrlEncoded
    @PUT("login")
    Call<Void> stop(@Field("id") long id);

    @FormUrlEncoded
    @PUT("login")
    Call<Void> go_to(@Field("id") long id, @Field("time") long time);
}
