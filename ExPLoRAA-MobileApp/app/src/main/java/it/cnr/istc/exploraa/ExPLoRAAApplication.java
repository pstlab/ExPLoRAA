package it.cnr.istc.exploraa;

import android.app.Application;
import android.content.Intent;
import android.util.Log;

public class ExPLoRAAApplication extends Application {

    private static final String TAG = "ExPLoRAAApplication";
    private final Intent intent = new Intent(this, ExPLoRAAService.class);
    private boolean serviceRunning = false;

    @Override
    public void onCreate() {
        super.onCreate();
        Log.i(TAG, "Creating ExPLoRAA Application..");
    }

    public boolean isServiceRunning() {
        return serviceRunning;
    }

    public void setServiceRunning(boolean serviceRunning) {
        this.serviceRunning = serviceRunning;
    }
}
