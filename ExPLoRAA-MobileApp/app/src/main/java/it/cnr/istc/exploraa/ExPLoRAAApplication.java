package it.cnr.istc.exploraa;

import android.app.Application;
import android.content.Intent;
import android.os.Build;
import android.util.Log;

public class ExPLoRAAApplication extends Application {

    private static final String TAG = "ExPLoRAAApplication";

    @Override
    public void onCreate() {
        super.onCreate();
        Log.i(TAG, "Creating ExPLoRAA Application..");

        final Intent intent = new Intent(this, ExPLoRAAService.class);
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) startForegroundService(intent);
        else startService(intent);
    }
}
