package it.cnr.istc.pst.exploraa.mobile;

import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.content.Intent;
import android.content.SharedPreferences;
import android.os.Bundle;
import android.util.Log;

import androidx.appcompat.app.AppCompatActivity;
import androidx.preference.PreferenceManager;

import it.cnr.istc.pst.exploraa.mobile.ctx.ExPLoRAAContext;

public class MainActivity extends AppCompatActivity {

    private SharedPreferences shared_prefs;

    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        NotificationManager nm = (NotificationManager) getSystemService(NOTIFICATION_SERVICE);
        nm.createNotificationChannel(new NotificationChannel(getString(R.string.app_name), getString(R.string.app_name), NotificationManager.IMPORTANCE_DEFAULT));

        if (ExPLoRAAContext.getInstance().getUser() == null) {
            Log.i(MainActivity.class.getName(), "There is no current user..");
            shared_prefs = PreferenceManager.getDefaultSharedPreferences(this);
            if (shared_prefs.contains(getString(R.string.email)) && shared_prefs.contains(getString(R.string.password))) {
                Log.i(MainActivity.class.getName(), "There are stored credentials, we try to use them to log in..");
                ExPLoRAAContext.getInstance().login(this, shared_prefs.getString(getString(R.string.email), null), shared_prefs.getString(getString(R.string.password), null));
            } else {
                Log.i(MainActivity.class.getName(), "There are no stored credentials, we go to the login activity..");
                startActivity(new Intent(MainActivity.this, LoginActivity.class));
                finish();
            }
        }
    }
}
