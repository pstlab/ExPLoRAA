package it.cnr.istc.pst.exploraa.mobile;

import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.preference.PreferenceManager;
import android.util.Log;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.appcompat.app.AppCompatActivity;

import org.eclipse.paho.client.mqttv3.IMqttDeliveryToken;
import org.eclipse.paho.client.mqttv3.MqttCallback;
import org.eclipse.paho.client.mqttv3.MqttClient;
import org.eclipse.paho.client.mqttv3.MqttConnectOptions;
import org.eclipse.paho.client.mqttv3.MqttException;
import org.eclipse.paho.client.mqttv3.MqttMessage;
import org.eclipse.paho.client.mqttv3.persist.MemoryPersistence;

import java.io.IOException;
import java.security.SecureRandom;
import java.security.cert.CertificateException;

import javax.net.ssl.HostnameVerifier;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLSession;
import javax.net.ssl.SSLSocketFactory;
import javax.net.ssl.TrustManager;
import javax.net.ssl.X509TrustManager;

import it.cnr.istc.pst.exploraa.api.User;
import okhttp3.OkHttpClient;
import retrofit2.Call;
import retrofit2.Callback;
import retrofit2.Response;
import retrofit2.Retrofit;
import retrofit2.converter.jackson.JacksonConverterFactory;

public class ExPContext {

    private static final ExPContext instance = new ExPContext();
    private ExPLoRAA resource;
    private MqttClient mqtt;
    /**
     * The current user.
     */
    private User user;

    private ExPContext() {
        Log.i(ExPContext.class.getName(), "Creating ExPLoRAA context..");
        Retrofit retrofit = new Retrofit.Builder().baseUrl("https://" + BuildConfig.HOST + ":" + BuildConfig.SERVICE_PORT + "/").client(getUnsafeOkHttpClient()).addConverterFactory(JacksonConverterFactory.create()).build();
        resource = retrofit.create(ExPLoRAA.class);
    }

    public static ExPContext getInstance() {
        return instance;
    }

    private static OkHttpClient getUnsafeOkHttpClient() {
        try {
            // Create a trust manager that does not validate certificate chains
            final TrustManager[] trustAllCerts = new TrustManager[]{
                    new X509TrustManager() {
                        @Override
                        public void checkClientTrusted(java.security.cert.X509Certificate[] chain, String authType) throws CertificateException {
                        }

                        @Override
                        public void checkServerTrusted(java.security.cert.X509Certificate[] chain, String authType) throws CertificateException {
                        }

                        @Override
                        public java.security.cert.X509Certificate[] getAcceptedIssuers() {
                            return new java.security.cert.X509Certificate[]{};
                        }
                    }
            };

            // Install the all-trusting trust manager
            final SSLContext sslContext = SSLContext.getInstance("SSL");
            sslContext.init(null, trustAllCerts, new java.security.SecureRandom());

            // Create an ssl socket factory with our all-trusting manager
            final SSLSocketFactory sslSocketFactory = sslContext.getSocketFactory();

            OkHttpClient.Builder builder = new OkHttpClient.Builder();
            builder.sslSocketFactory(sslSocketFactory, (X509TrustManager) trustAllCerts[0]);
            builder.hostnameVerifier(new HostnameVerifier() {
                @Override
                public boolean verify(String hostname, SSLSession session) {
                    return true;
                }
            });

            return builder.build();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Logs into the ExPLoRAA service, initializing stimuli, followed lessons, teachers, teached lessons and students.
     *
     * @param ctx      the invoking context.
     * @param email    the email which identifies the user.
     * @param password the password of the user.
     */
    public void login(@NonNull final Context ctx, @NonNull final String email, @NonNull final String password) {
        Log.i(ExPContext.class.getName(), "Logging in..");
        resource.login(email, password).enqueue(new Callback<User>() {
            @Override
            public void onResponse(Call<User> call, Response<User> response) {
                if (response.isSuccessful()) {
                    Log.i(ExPContext.class.getName(), "Login successful..");

                    // we store email and password so as to avoid asking them every time the app is started..
                    SharedPreferences shared_prefs = PreferenceManager.getDefaultSharedPreferences(ctx);
                    SharedPreferences.Editor prefs_edit = shared_prefs.edit();
                    prefs_edit.putString(ctx.getString(R.string.email), email);
                    prefs_edit.putString(ctx.getString(R.string.password), password);
                    prefs_edit.apply();

                    setUser(ctx, response.body());
                } else {
                    try {
                        Toast.makeText(ctx, response.errorBody().string(), Toast.LENGTH_SHORT).show();
                    } catch (IOException e) {
                        Log.e(ExPContext.class.getName(), null, e);
                    }
                }
            }

            @Override
            public void onFailure(Call<User> call, Throwable t) {
                Log.e(ExPContext.class.getName(), "User login failed..", t);
            }
        });
    }

    /**
     * Logs out from the ExPLoRAA service, clearing stimuli, followed lessons, teachers, teached lessons and students.
     */
    public void logout(@NonNull final Context ctx) {
        assert user != null;
        Log.i(ExPContext.class.getName(), "Logging out current user..");
        setUser(ctx, null);
    }

    public void new_user(@NonNull final Context ctx, @NonNull final String email, @NonNull final String password, @NonNull final String first_name, @NonNull final String last_name) {
        Log.i(ExPContext.class.getName(), "Creating new user..");
        resource.new_user(email, password, first_name, last_name).enqueue(new Callback<Void>() {
            @Override
            public void onResponse(Call<Void> call, Response<Void> response) {
                if (response.isSuccessful()) {
                    Log.i(ExPContext.class.getName(), "User creation successful..");
                    login(ctx, email, password);
                } else {
                    try {
                        Toast.makeText(ctx, response.errorBody().string(), Toast.LENGTH_SHORT).show();
                    } catch (IOException e) {
                        Log.e(ExPContext.class.getName(), null, e);
                    }
                }
            }

            @Override
            public void onFailure(Call<Void> call, Throwable t) {
                Log.e(ExPContext.class.getName(), "User login failed..", t);
            }
        });
    }

    public User getUser() {
        return user;
    }

    private void setUser(@NonNull final Context ctx, @NonNull final User user) {
        if (this.user != user) {
            if (this.user != null) {
                // TODO: we make some cleanings..

                try {
                    if (mqtt.isConnected())
                        mqtt.disconnect();
                    mqtt.close();
                    Log.i(ExPContext.class.getName(), "Disconnected from the MQTT broker..");
                } catch (MqttException e) {
                    Log.w(ExPContext.class.getName(), "MQTT Disconnection failed..", e);
                } finally {
                    mqtt = null;
                }
            }

            if (user != null) {
                // we create a new MQTT connection..
                try {
                    mqtt = new MqttClient("ssl://" + BuildConfig.HOST + ":" + BuildConfig.MQTT_PORT, "user-" + user.getId(), new MemoryPersistence());
                    mqtt.setCallback(new MqttCallback() {
                        @Override
                        public void connectionLost(Throwable cause) {
                            Log.e(ExPContext.class.getName(), "Connection lost..", cause);
                            logout(ctx);
                        }

                        @Override
                        public void messageArrived(String topic, MqttMessage message) {
                            Log.w(ExPContext.class.getName(), "Message arrived: " + topic + " - " + message);
                        }

                        @Override
                        public void deliveryComplete(IMqttDeliveryToken token) {
                        }
                    });

                    // Create a trust manager that does not validate certificate chains
                    final TrustManager[] trustAllCerts = new TrustManager[]{
                            new X509TrustManager() {
                                @Override
                                public void checkClientTrusted(java.security.cert.X509Certificate[] chain, String authType) throws CertificateException {
                                }

                                @Override
                                public void checkServerTrusted(java.security.cert.X509Certificate[] chain, String authType) throws CertificateException {
                                }

                                @Override
                                public java.security.cert.X509Certificate[] getAcceptedIssuers() {
                                    return new java.security.cert.X509Certificate[]{};
                                }
                            }
                    };

                    SSLContext sslContext = SSLContext.getInstance("TLS");
                    sslContext.init(null, trustAllCerts, new SecureRandom());

                    MqttConnectOptions options = new MqttConnectOptions();
                    options.setCleanSession(true);
                    options.setSocketFactory(sslContext.getSocketFactory());
                    options.setAutomaticReconnect(true);
                    mqtt.connect(options);
                    Log.i(ExPContext.class.getName(), "Connected to the MQTT broker..");

                    ctx.startActivity(new Intent(ctx, ExPLoRAAActivity.class));
                    ((AppCompatActivity) ctx).finish();
                } catch (Exception e) {
                    Log.w(ExPContext.class.getName(), "MQTT Connection failed..", e);
                }
            }

            this.user = user;
        }
    }
}
