package it.cnr.istc.exploraa;

import android.Manifest;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.pm.PackageManager;
import android.os.Bundle;
import android.preference.PreferenceManager;
import android.support.annotation.NonNull;
import android.support.v4.app.ActivityCompat;
import android.support.v4.content.ContextCompat;
import android.support.v7.app.AppCompatActivity;
import android.util.Log;
import android.view.KeyEvent;
import android.view.View;
import android.widget.EditText;

import java.util.concurrent.ExecutionException;

public class NewUserActivity extends AppCompatActivity {

    public static final String TAG = "LoginActivity";
    public static final int ACCESS_FINE_LOCATION_REQUEST_CODE_ASK_PERMISSIONS = 123;
    private EditText email;
    private EditText password;
    private EditText first_name;
    private EditText last_name;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_new_user);

        email = findViewById(R.id.new_user_input_email);
        password = findViewById(R.id.new_user_input_password);
        first_name = findViewById(R.id.new_user_input_first_name);
        last_name = findViewById(R.id.new_user_input_last_name);
    }

    @Override
    public boolean onKeyDown(int keyCode, KeyEvent event) {
        if ((keyCode == KeyEvent.KEYCODE_BACK)) {
            startActivity(new Intent(this, LoginActivity.class));
            finish();
        }
        return super.onKeyDown(keyCode, event);
    }

    public void new_user(View v) {
        if (ContextCompat.checkSelfPermission(this, Manifest.permission.ACCESS_FINE_LOCATION) != PackageManager.PERMISSION_GRANTED) {
            ActivityCompat.requestPermissions(this, new String[]{Manifest.permission.ACCESS_FINE_LOCATION}, ACCESS_FINE_LOCATION_REQUEST_CODE_ASK_PERMISSIONS);
        } else {
            try {
                if (ExPLoRAAContext.getInstance().new_user(this, email.getText().toString(), password.getText().toString(), first_name.getText().toString(), last_name.getText().toString())) {
                    // we store email and password so as to avoid asking them everytime the app is started..
                    SharedPreferences shared_prefs = PreferenceManager.getDefaultSharedPreferences(this);
                    SharedPreferences.Editor prefs_edit = shared_prefs.edit();
                    prefs_edit.putString(getString(R.string.email), email.getText().toString());
                    prefs_edit.putString(getString(R.string.password), password.getText().toString());
                    prefs_edit.apply();

                    startActivity(new Intent(this, MainActivity.class));
                    finish();
                }
            } catch (ExecutionException | InterruptedException e) {
                Log.e(TAG, "user creation failed..", e);
            }
        }
    }

    @Override
    public void onRequestPermissionsResult(int requestCode, @NonNull String[] permissions, @NonNull int[] grantResults) {
        switch (requestCode) {
            case ACCESS_FINE_LOCATION_REQUEST_CODE_ASK_PERMISSIONS: {
                if (grantResults.length > 0 && grantResults[0] == PackageManager.PERMISSION_GRANTED) {
                    try {
                        if (ExPLoRAAContext.getInstance().new_user(this, email.getText().toString(), password.getText().toString(), first_name.getText().toString(), last_name.getText().toString())) {
                            // we store email and password so as to avoid asking them everytime the app is started..
                            SharedPreferences shared_prefs = PreferenceManager.getDefaultSharedPreferences(this);
                            SharedPreferences.Editor prefs_edit = shared_prefs.edit();
                            prefs_edit.putString(getString(R.string.email), email.getText().toString());
                            prefs_edit.putString(getString(R.string.password), password.getText().toString());
                            prefs_edit.apply();

                            startActivity(new Intent(this, MainActivity.class));
                            finish();
                        }
                    } catch (ExecutionException | InterruptedException e) {
                        Log.e(TAG, "user creation failed..", e);
                    }
                }
            }
        }
    }
}
