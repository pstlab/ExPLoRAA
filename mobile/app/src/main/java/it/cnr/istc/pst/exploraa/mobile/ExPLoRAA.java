package it.cnr.istc.pst.exploraa.mobile;

import it.cnr.istc.pst.exploraa.api.User;
import retrofit2.Call;
import retrofit2.http.DELETE;
import retrofit2.http.Field;
import retrofit2.http.FormUrlEncoded;
import retrofit2.http.Header;
import retrofit2.http.POST;
import retrofit2.http.Path;

public interface ExPLoRAA {

    @FormUrlEncoded
    @POST("login")
    Call<User> login(@Field("email") String email, @Field("password") String password);

    @DELETE("user/{id}")
    void delete_user(@Header("Authorization") String auth, @Path("id") long id);
}
