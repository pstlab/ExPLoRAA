package it.cnr.istc.pst.exploraa.mobile;

import android.content.Intent;
import android.os.Bundle;
import android.view.View;
import android.widget.EditText;

import androidx.appcompat.app.AppCompatActivity;

public class LoginActivity extends AppCompatActivity {

    private EditText email;
    private EditText password;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_login);

        email = findViewById(R.id.login_input_email);
        password = findViewById(R.id.login_input_password);
    }

    public void login(View v) {
        ExPContext.getInstance().login(this, email.getText().toString(), password.getText().toString());
    }

    public void new_user(View v) {
        startActivity(new Intent(this, NewUserActivity.class));
        finish();
    }
}
