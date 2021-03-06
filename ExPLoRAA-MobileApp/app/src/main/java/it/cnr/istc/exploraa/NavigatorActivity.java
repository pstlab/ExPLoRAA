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

/**
 * @author Riccardo De Benedictis
 */
public class NavigatorActivity extends AppCompatActivity implements ExPLoRAAContext.ServiceListener {

    public static final String TAG = "NavigatorActivity";
    public static final int ACCESS_FINE_LOCATION_REQUEST_CODE_ASK_PERMISSIONS = 123;
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
        ExPLoRAAContext.getInstance().addServiceListener(this);

        registerReceiver(login_receiver, new IntentFilter(ExPLoRAAService.LOGIN));

        if (ExPLoRAAContext.getInstance().isServiceRunning()) {
            if (ExPLoRAAContext.getInstance().getService().getUser() == null) {
                SharedPreferences shared_prefs = PreferenceManager.getDefaultSharedPreferences(this);
                if (shared_prefs.contains(getString(R.string.email)) && shared_prefs.contains(getString(R.string.password)))
                    ExPLoRAAContext.getInstance().getService().login(shared_prefs.getString(getString(R.string.email), null), shared_prefs.getString(getString(R.string.password), null));
            } else {
                startActivity(new Intent(NavigatorActivity.this, MainActivity.class));
                finish();
            }
        } else {
            SharedPreferences shared_prefs = PreferenceManager.getDefaultSharedPreferences(NavigatorActivity.this);
            if (!shared_prefs.contains(getString(R.string.email)) || !shared_prefs.contains(getString(R.string.password))) {
                startActivity(new Intent(NavigatorActivity.this, LoginActivity.class));
                finish();
            } else if (ContextCompat.checkSelfPermission(NavigatorActivity.this, Manifest.permission.ACCESS_FINE_LOCATION) != PackageManager.PERMISSION_GRANTED)
                ActivityCompat.requestPermissions(NavigatorActivity.this, new String[]{Manifest.permission.ACCESS_FINE_LOCATION}, ACCESS_FINE_LOCATION_REQUEST_CODE_ASK_PERMISSIONS);
            else ExPLoRAAContext.getInstance().startService(this);
        }
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        ExPLoRAAContext.getInstance().removeServiceListener(this);
        unregisterReceiver(login_receiver);
    }

    @Override
    public void onRequestPermissionsResult(int requestCode, @NonNull String[] permissions, @NonNull int[] grantResults) {
        switch (requestCode) {
            case ACCESS_FINE_LOCATION_REQUEST_CODE_ASK_PERMISSIONS:
                if (grantResults.length > 0 && grantResults[0] == PackageManager.PERMISSION_GRANTED)
                    ExPLoRAAContext.getInstance().startService(this);
                else
                    finish();
        }
    }

    @Override
    public void serviceConnected(ExPLoRAAService service) {
        SharedPreferences shared_prefs = PreferenceManager.getDefaultSharedPreferences(service);
        if (shared_prefs.contains(service.getString(R.string.email)) && shared_prefs.contains(service.getString(R.string.password)))
            service.login(shared_prefs.getString(service.getString(R.string.email), null), shared_prefs.getString(service.getString(R.string.password), null));
    }

    @Override
    public void serviceDisonnected() {
        finish();
    }
}
