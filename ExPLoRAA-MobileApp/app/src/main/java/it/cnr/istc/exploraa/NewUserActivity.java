package it.cnr.istc.exploraa;

import android.Manifest;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.SharedPreferences;
import android.content.pm.PackageManager;
import android.os.Bundle;
import android.preference.PreferenceManager;
import android.support.annotation.NonNull;
import android.support.v4.app.ActivityCompat;
import android.support.v4.content.ContextCompat;
import android.support.v7.app.AppCompatActivity;
import android.view.View;
import android.widget.EditText;

public class NewUserActivity extends AppCompatActivity implements ExPLoRAAContext.ServiceListener {

    public static final String TAG = "LoginActivity";
    public static final int ACCESS_FINE_LOCATION_REQUEST_CODE_ASK_PERMISSIONS = 123;
    private EditText email;
    private EditText password;
    private EditText first_name;
    private EditText last_name;
    private BroadcastReceiver user_creation_receiver = new BroadcastReceiver() {
        @Override
        public void onReceive(Context context, Intent intent) {
            if (intent.getBooleanExtra("successful", false)) {
                // we store email and password so as to avoid asking them every time the app is started..
                SharedPreferences shared_prefs = PreferenceManager.getDefaultSharedPreferences(NewUserActivity.this);
                SharedPreferences.Editor prefs_edit = shared_prefs.edit();
                prefs_edit.putString(getString(R.string.email), email.getText().toString());
                prefs_edit.putString(getString(R.string.password), password.getText().toString());
                prefs_edit.apply();

                startActivity(new Intent(NewUserActivity.this, MainActivity.class));
                finish();
            }
        }
    };

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_new_user);

        email = findViewById(R.id.new_user_input_email);
        password = findViewById(R.id.new_user_input_password);
        first_name = findViewById(R.id.new_user_input_first_name);
        last_name = findViewById(R.id.new_user_input_last_name);

        registerReceiver(user_creation_receiver, new IntentFilter(ExPLoRAAService.USER_CREATION));

        ExPLoRAAContext.getInstance().addServiceListener(this);
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        unregisterReceiver(user_creation_receiver);
        ExPLoRAAContext.getInstance().removeServiceListener(this);
    }

    @Override
    public void onBackPressed() {
        startActivity(new Intent(this, LoginActivity.class));
        finish();
        super.onBackPressed();
    }

    public void new_user(View v) {
        if (ContextCompat.checkSelfPermission(this, Manifest.permission.ACCESS_FINE_LOCATION) != PackageManager.PERMISSION_GRANTED)
            ActivityCompat.requestPermissions(this, new String[]{Manifest.permission.ACCESS_FINE_LOCATION}, ACCESS_FINE_LOCATION_REQUEST_CODE_ASK_PERMISSIONS);
        else
            ExPLoRAAContext.getInstance().startService(getApplication());
    }

    @Override
    public void onRequestPermissionsResult(int requestCode, @NonNull String[] permissions, @NonNull int[] grantResults) {
        switch (requestCode) {
            case ACCESS_FINE_LOCATION_REQUEST_CODE_ASK_PERMISSIONS:
                ExPLoRAAContext.getInstance().startService(getApplication());
        }
    }

    @Override
    public void serviceConnected(ExPLoRAAService service) {
        service.new_user(email.getText().toString(), password.getText().toString(), first_name.getText().toString(), last_name.getText().toString());
    }

    @Override
    public void serviceDisonnected() {
    }
}
