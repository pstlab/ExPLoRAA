package it.cnr.istc.exploraa;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.os.Build;
import android.util.Log;

public class ExPLoRAAServiceReceiver extends BroadcastReceiver {

    private static final String TAG = "ExPLoRAAServiceReceiver";

    @Override
    public void onReceive(Context context, Intent intent) {
        Log.i(TAG, "Received " + intent.getAction() + " intent..");
        // we start the ExPLoRAA service..
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            context.startForegroundService(new Intent(context, ExPLoRAAService.class));
        } else {
            context.startService(new Intent(context, ExPLoRAAService.class));
        }
    }
}
