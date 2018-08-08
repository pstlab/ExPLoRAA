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

/**
 * @author Riccardo De Benedictis
 */
public class NavigatorActivity extends AppCompatActivity {

    public static final String TAG = "NavigatorActivity";
    public static final int ACCESS_FINE_LOCATION_REQUEST_CODE_ASK_PERMISSIONS = 123;
    private ExPLoRAAService service;
    private ServiceConnection service_connection = new ServiceConnection() {
        @Override
        public void onServiceConnected(ComponentName name, IBinder binder) {
            service = ((ExPLoRAAService.ExPLoRAABinder) binder).getService();

            if (service.getUser() != null) {
                Log.i(TAG, "User is already logged in..");
                startActivity(new Intent(NavigatorActivity.this, MainActivity.class));
                finish();
            } else {
                SharedPreferences shared_prefs = PreferenceManager.getDefaultSharedPreferences(NavigatorActivity.this);
                if (!shared_prefs.contains(getString(R.string.email)) || !shared_prefs.contains(getString(R.string.password))) {
                    startActivity(new Intent(NavigatorActivity.this, LoginActivity.class));
                    finish();
                } else if (ContextCompat.checkSelfPermission(NavigatorActivity.this, Manifest.permission.ACCESS_FINE_LOCATION) != PackageManager.PERMISSION_GRANTED)
                    ActivityCompat.requestPermissions(NavigatorActivity.this, new String[]{Manifest.permission.ACCESS_FINE_LOCATION}, ACCESS_FINE_LOCATION_REQUEST_CODE_ASK_PERMISSIONS);
            }
        }

        @Override
        public void onServiceDisconnected(ComponentName name) {
            service = null;
        }
    };
    private BroadcastReceiver login_receiver = new BroadcastReceiver() {
        @Override
        public void onReceive(Context context, Intent intent) {
            if (intent.getBooleanExtra("successful", false))
                startActivity(new Intent(NavigatorActivity.this, MainActivity.class));
            else startActivity(new Intent(NavigatorActivity.this, LoginActivity.class));
            finish();
        }
    };

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        registerReceiver(login_receiver, new IntentFilter(ExPLoRAAService.LOGIN));

        // we bind the ExPLoRAA service..
        if (!bindService(new Intent(this, ExPLoRAAService.class), service_connection, Context.BIND_AUTO_CREATE))
            Log.e(TAG, "Error: The requested service doesn't exist, or this client isn't allowed access to it.");
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        if (service_connection != null) {
            unbindService(service_connection);
        }
        unregisterReceiver(login_receiver);
    }

    @Override
    public void onRequestPermissionsResult(int requestCode, @NonNull String[] permissions, @NonNull int[] grantResults) {
        switch (requestCode) {
            case ACCESS_FINE_LOCATION_REQUEST_CODE_ASK_PERMISSIONS: {
                if (grantResults.length > 0 && grantResults[0] == PackageManager.PERMISSION_GRANTED) {
                    SharedPreferences shared_prefs = PreferenceManager.getDefaultSharedPreferences(this);
                    service.login(shared_prefs.getString(getString(R.string.email), null), shared_prefs.getString(getString(R.string.password), null));
                } else
                    finish();
            }
        }
    }
}
