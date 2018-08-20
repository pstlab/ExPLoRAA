package it.cnr.istc.exploraa;

import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.ServiceConnection;
import android.content.SharedPreferences;
import android.os.Build;
import android.os.IBinder;
import android.preference.PreferenceManager;
import android.support.annotation.NonNull;
import android.util.Log;

import java.util.ArrayList;
import java.util.Collection;

public class ExPLoRAAContext {

    private static ExPLoRAAContext instance;

    public static ExPLoRAAContext getInstance() {
        if (instance == null) instance = new ExPLoRAAContext();
        return instance;
    }

    private static final String TAG = "ExPLoRAAApplication";
    private ExPLoRAAService service;
    private ServiceConnection service_connection = new ServiceConnection() {
        @Override
        public void onServiceConnected(ComponentName name, IBinder binder) {
            service = ((ExPLoRAAService.ExPLoRAABinder) binder).getService();

            for (ServiceListener l : service_listeners)
                l.serviceConnected(service);

            SharedPreferences shared_prefs = PreferenceManager.getDefaultSharedPreferences(service);
            if (shared_prefs.contains(service.getString(R.string.email)) && shared_prefs.contains(service.getString(R.string.password)))
                service.login(shared_prefs.getString(service.getString(R.string.email), null), shared_prefs.getString(service.getString(R.string.password), null));
        }

        @Override
        public void onServiceDisconnected(ComponentName name) {
            service = null;
            for (ServiceListener l : service_listeners) l.serviceDisonnected();
        }
    };
    private final Collection<ServiceListener> service_listeners = new ArrayList<>();

    public boolean isServiceRunning() {
        return service != null;
    }

    public ExPLoRAAService getService() {
        return service;
    }

    public void startService(@NonNull Context ctx) {
        Log.i(TAG, "Starting service..");
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O)
            ctx.getApplicationContext().startForegroundService(new Intent(ctx.getApplicationContext(), ExPLoRAAService.class));
        else
            ctx.getApplicationContext().startService(new Intent(ctx.getApplicationContext(), ExPLoRAAService.class));
        if (!ctx.getApplicationContext().bindService(new Intent(ctx.getApplicationContext(), ExPLoRAAService.class), service_connection, Context.BIND_AUTO_CREATE))
            Log.e(TAG, "Error: The requested service doesn't exist, or this client isn't allowed access to it.");
    }

    public void stopService(@NonNull Context ctx) {
        Log.i(TAG, "Stopping service..");
        assert service != null;
        ctx.getApplicationContext().unbindService(service_connection);
        ctx.getApplicationContext().stopService(new Intent(ctx.getApplicationContext(), ExPLoRAAService.class));
    }

    public void addServiceListener(ServiceListener l) {
        service_listeners.add(l);
    }

    public void removeServiceListener(ServiceListener l) {
        service_listeners.remove(l);
    }

    public interface ServiceListener {

        void serviceConnected(ExPLoRAAService service);

        void serviceDisonnected();
    }
}
