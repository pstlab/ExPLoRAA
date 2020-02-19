package it.cnr.istc.pst.exploraa.mobile;

import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.content.Intent;
import android.content.SharedPreferences;
import android.os.Bundle;

import androidx.appcompat.app.AppCompatActivity;
import androidx.preference.PreferenceManager;

public class MainActivity extends AppCompatActivity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        NotificationManager nm = (NotificationManager) getSystemService(NOTIFICATION_SERVICE);
        nm.createNotificationChannel(new NotificationChannel(getString(R.string.app_name), getString(R.string.app_name), NotificationManager.IMPORTANCE_DEFAULT));

        if (ExPContext.getInstance().getUser() == null) {
            SharedPreferences shared_prefs = PreferenceManager.getDefaultSharedPreferences(this);
            if (shared_prefs.contains(getString(R.string.email)) && shared_prefs.contains(getString(R.string.password)))
                ExPContext.getInstance().login(this, shared_prefs.getString(getString(R.string.email), null), shared_prefs.getString(getString(R.string.password), null));
            else {
                startActivity(new Intent(MainActivity.this, LoginActivity.class));
                finish();
            }
        }
    }
}
