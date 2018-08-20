package it.cnr.istc.exploraa;

import android.os.Bundle;
import android.support.v7.app.AppCompatActivity;
import android.widget.TextView;

public class StudentActivity extends AppCompatActivity implements StudentContext.StudentListener {

    private StudentContext ctx;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_student);

        TextView student_first_name = findViewById(R.id.student_first_name);
        TextView student_last_name = findViewById(R.id.student_last_name);

        long student_id = getIntent().getLongExtra("student_id", -1);
        ctx = ExPLoRAAContext.getInstance().getService().getStudent(student_id);
        ctx.addListener(this);

        student_first_name.setText(ctx.getStudent().first_name);
        student_last_name.setText(ctx.getStudent().last_name);
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        ctx.removeListener(this);
    }

    @Override
    public void onlineChanged(boolean on_line) {

    }
}
