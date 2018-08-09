package it.cnr.istc.exploraa;

import android.Manifest;
import android.content.BroadcastReceiver;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.ServiceConnection;
import android.content.SharedPreferences;
import android.content.pm.PackageManager;
import android.os.Bundle;
import android.os.IBinder;
import android.preference.PreferenceManager;
import android.support.annotation.NonNull;
import android.support.v4.app.ActivityCompat;
import android.support.v4.content.ContextCompat;
import android.support.v7.app.AppCompatActivity;
import android.util.Log;
import android.view.View;
import android.widget.EditText;

public class LoginActivity extends AppCompatActivity {

    public static final String TAG = "LoginActivity";
    public static final int ACCESS_FINE_LOCATION_REQUEST_CODE_ASK_PERMISSIONS = 123;
    private EditText email;
    private EditText password;
    private ExPLoRAAService service;
    private ServiceConnection service_connection = new ServiceConnection() {
        @Override
        public void onServiceConnected(ComponentName name, IBinder binder) {
            service = ((ExPLoRAAService.ExPLoRAABinder) binder).getService();
            service.login(email.getText().toString(), password.getText().toString());
        }

        @Override
        public void onServiceDisconnected(ComponentName name) {
            service = null;
        }
    };
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
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        if (service != null) {
            unbindService(service_connection);
        }
        unregisterReceiver(login_receiver);
    }

    public void login(View v) {
        if (ContextCompat.checkSelfPermission(this, Manifest.permission.ACCESS_FINE_LOCATION) != PackageManager.PERMISSION_GRANTED)
            ActivityCompat.requestPermissions(this, new String[]{Manifest.permission.ACCESS_FINE_LOCATION}, ACCESS_FINE_LOCATION_REQUEST_CODE_ASK_PERMISSIONS);
        else {
            startService(new Intent(this, ExPLoRAAService.class));
            ((ExPLoRAAApplication) getApplication()).setServiceRunning(true);
            // we bind the ExPLoRAA service..
            if (!bindService(new Intent(this, ExPLoRAAService.class), service_connection, Context.BIND_AUTO_CREATE))
                Log.e(TAG, "Error: The requested service doesn't exist, or this client isn't allowed access to it.");
        }
    }

    public void new_user(View v) {
        startActivity(new Intent(this, NewUserActivity.class));
        finish();
    }

    @Override
    public void onRequestPermissionsResult(int requestCode, @NonNull String[] permissions, @NonNull int[] grantResults) {
        switch (requestCode) {
            case ACCESS_FINE_LOCATION_REQUEST_CODE_ASK_PERMISSIONS: {
                if (grantResults.length > 0 && grantResults[0] == PackageManager.PERMISSION_GRANTED) {
                    startService(new Intent(this, ExPLoRAAService.class));
                    ((ExPLoRAAApplication) getApplication()).setServiceRunning(true);
                    // we bind the ExPLoRAA service..
                    if (!bindService(new Intent(this, ExPLoRAAService.class), service_connection, Context.BIND_AUTO_CREATE))
                        Log.e(TAG, "Error: The requested service doesn't exist, or this client isn't allowed access to it.");
                }
            }
        }
    }
}
