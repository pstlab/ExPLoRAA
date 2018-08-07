package it.cnr.istc.exploraa;

import android.content.BroadcastReceiver;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.ServiceConnection;
import android.os.Bundle;
import android.os.IBinder;
import android.support.v7.app.AppCompatActivity;
import android.util.Log;
import android.widget.TextView;

public class StudentActivity extends AppCompatActivity {

    private static final String TAG = "StudentActivity";
    private long student_id;
    private StudentContext ctx;
    private TextView student_first_name;
    private TextView student_last_name;
    private ExPLoRAAService service;
    private ServiceConnection service_connection = new ServiceConnection() {
        @Override
        public void onServiceConnected(ComponentName name, IBinder binder) {
            service = ((ExPLoRAAService.ExPLoRAABinder) binder).getService();

            ctx = service.getStudent(student_id);

            student_first_name.setText(ctx.getStudent().first_name);
            student_last_name.setText(ctx.getStudent().last_name);
        }

        @Override
        public void onServiceDisconnected(ComponentName name) {
            service = null;
        }
    };
    private BroadcastReceiver online_receiver = new BroadcastReceiver() {
        @Override
        public void onReceive(Context context, Intent intent) {
        }
    };

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_student);

        student_first_name = findViewById(R.id.student_first_name);
        student_last_name = findViewById(R.id.student_last_name);

        student_id = getIntent().getLongExtra("student_id", -1);

        registerReceiver(online_receiver, new IntentFilter(StudentContext.STUDENT_ONLINE + student_id));

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
        unregisterReceiver(online_receiver);
    }
}
