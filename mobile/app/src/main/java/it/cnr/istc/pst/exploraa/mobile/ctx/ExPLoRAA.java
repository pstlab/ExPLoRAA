package it.cnr.istc.pst.exploraa.mobile.ctx;

import java.util.Collection;

import it.cnr.istc.pst.exploraa.api.Lesson;
import it.cnr.istc.pst.exploraa.api.User;
import retrofit2.Call;
import retrofit2.http.DELETE;
import retrofit2.http.Field;
import retrofit2.http.FormUrlEncoded;
import retrofit2.http.GET;
import retrofit2.http.Header;
import retrofit2.http.POST;
import retrofit2.http.Path;

public interface ExPLoRAA {

    @FormUrlEncoded
    @POST("login")
    Call<User> login(@Field("email") String email, @Field("password") String password);

    @FormUrlEncoded
    @POST("users")
    Call<Void> new_user(@Field("email") String email, @Field("password") String password, @Field("first_name") String first_name, @Field("last_name") String last_name);

    @DELETE("user/{id}")
    Call<Void> delete_user(@Header("Authorization") String auth, @Path("id") long id);

    @GET("lessons")
    Call<Collection<Lesson>> get_lessons();
}
