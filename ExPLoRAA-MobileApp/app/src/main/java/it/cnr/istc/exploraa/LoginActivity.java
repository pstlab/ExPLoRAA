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

public class LoginActivity extends AppCompatActivity implements ExPLoRAAContext.ServiceListener {

    public static final String TAG = "LoginActivity";
    public static final int ACCESS_FINE_LOCATION_REQUEST_CODE_ASK_PERMISSIONS = 123;
    private EditText email;
    private EditText password;
    private BroadcastReceiver login_receiver = new BroadcastReceiver() {
        @Override
        public void onReceive(Context context, Intent intent) {
            if (intent.getBooleanExtra("successful", false)) {
                // we store email and password so as to avoid asking them every time the app is started..
                SharedPreferences shared_prefs = PreferenceManager.getDefaultSharedPreferences(LoginActivity.this);
                SharedPreferences.Editor prefs_edit = shared_prefs.edit();
                prefs_edit.putString(getString(R.string.email), email.getText().toString());
                prefs_edit.putString(getString(R.string.password), password.getText().toString());
                prefs_edit.apply();

                startActivity(new Intent(LoginActivity.this, MainActivity.class));
                finish();
            }
        }
    };

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_login);

        email = findViewById(R.id.login_input_email);
        password = findViewById(R.id.login_input_password);

        registerReceiver(login_receiver, new IntentFilter(ExPLoRAAService.LOGIN));

        ExPLoRAAContext.getInstance().addServiceListener(this);
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        unregisterReceiver(login_receiver);
        ExPLoRAAContext.getInstance().removeServiceListener(this);
    }

    public void login(View v) {
        if (ContextCompat.checkSelfPermission(this, Manifest.permission.ACCESS_FINE_LOCATION) != PackageManager.PERMISSION_GRANTED)
            ActivityCompat.requestPermissions(this, new String[]{Manifest.permission.ACCESS_FINE_LOCATION}, ACCESS_FINE_LOCATION_REQUEST_CODE_ASK_PERMISSIONS);
        else ExPLoRAAContext.getInstance().startService(getApplication());
    }

    public void new_user(View v) {
        startActivity(new Intent(this, NewUserActivity.class));
        finish();
    }

    @Override
    public void onRequestPermissionsResult(int requestCode, @NonNull String[] permissions, @NonNull int[] grantResults) {
        switch (requestCode) {
            case ACCESS_FINE_LOCATION_REQUEST_CODE_ASK_PERMISSIONS:
                if (grantResults.length > 0 && grantResults[0] == PackageManager.PERMISSION_GRANTED)
                    ExPLoRAAContext.getInstance().startService(getApplication());
        }
    }

    @Override
    public void serviceConnected(ExPLoRAAService service) {
        service.login(email.getText().toString(), password.getText().toString());
    }

    @Override
    public void serviceDisonnected() {
    }
}
