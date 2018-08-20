package it.cnr.istc.exploraa;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.util.Log;

public class ExPLoRAAServiceReceiver extends BroadcastReceiver {

    private static final String TAG = "ExPLoRAAServiceReceiver";

    @Override
    public void onReceive(Context context, Intent intent) {
        Log.i(TAG, "Received " + intent.getAction() + " intent..");
        ExPLoRAAContext.getInstance().startService(context.getApplicationContext());
    }
}
