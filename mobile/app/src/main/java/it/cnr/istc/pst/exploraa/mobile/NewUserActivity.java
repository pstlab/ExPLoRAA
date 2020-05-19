package it.cnr.istc.pst.exploraa.mobile;

import android.Manifest;
import android.content.pm.PackageManager;
import android.os.Bundle;
import android.view.View;
import android.widget.EditText;

import androidx.annotation.NonNull;
import androidx.appcompat.app.AppCompatActivity;
import androidx.core.app.ActivityCompat;
import androidx.core.content.ContextCompat;

import it.cnr.istc.pst.exploraa.mobile.ctx.ExPLoRAAContext;

public class NewUserActivity extends AppCompatActivity {

    public static final int ACCESS_FINE_LOCATION_REQUEST_CODE_ASK_PERMISSIONS = 123;
    private EditText email;
    private EditText password;
    private EditText first_name;
    private EditText last_name;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_new_user);

        email = findViewById(R.id.new_user_input_email);
        password = findViewById(R.id.new_user_input_password);
        first_name = findViewById(R.id.new_user_input_first_name);
        last_name = findViewById(R.id.new_user_input_last_name);
    }

    public void new_user(View v) {
        if (ContextCompat.checkSelfPermission(this, Manifest.permission.ACCESS_FINE_LOCATION) == PackageManager.PERMISSION_GRANTED)
            ExPLoRAAContext.getInstance().new_user(this, email.getText().toString(), password.getText().toString(), first_name.getText().toString(), last_name.getText().toString());
        else
            ActivityCompat.requestPermissions(this, new String[]{Manifest.permission.ACCESS_FINE_LOCATION}, ACCESS_FINE_LOCATION_REQUEST_CODE_ASK_PERMISSIONS);
    }

    @Override
    public void onRequestPermissionsResult(int requestCode, @NonNull String[] permissions, @NonNull int[] grantResults) {
        switch (requestCode) {
            case ACCESS_FINE_LOCATION_REQUEST_CODE_ASK_PERMISSIONS:
                ExPLoRAAContext.getInstance().new_user(this, email.getText().toString(), password.getText().toString(), first_name.getText().toString(), last_name.getText().toString());
        }
    }
}
