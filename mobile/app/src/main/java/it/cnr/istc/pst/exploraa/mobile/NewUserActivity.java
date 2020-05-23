package it.cnr.istc.pst.exploraa.mobile;

import android.os.Bundle;
import android.view.View;
import android.widget.EditText;

import androidx.appcompat.app.AppCompatActivity;

import it.cnr.istc.pst.exploraa.mobile.ctx.ExPLoRAAContext;

public class NewUserActivity extends AppCompatActivity {

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
        ExPLoRAAContext.getInstance().new_user(this, email.getText().toString(), password.getText().toString(), first_name.getText().toString(), last_name.getText().toString());
    }
}
