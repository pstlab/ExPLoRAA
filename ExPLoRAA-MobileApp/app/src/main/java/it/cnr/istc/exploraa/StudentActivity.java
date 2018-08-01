package it.cnr.istc.exploraa;

import android.os.Bundle;
import android.support.v7.app.AppCompatActivity;
import android.widget.TextView;

public class StudentActivity extends AppCompatActivity implements StudentContext.StudentListener {

    private StudentContext ctx;
    private TextView student_first_name;
    private TextView student_last_name;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_student);

        student_first_name = findViewById(R.id.student_first_name);
        student_last_name = findViewById(R.id.student_last_name);

        long lesson_id = getIntent().getLongExtra("lesson_id", -1);
        ctx = ExPLoRAAContext.getInstance().getStudent(getIntent().getLongExtra("student_id", -1));

        student_first_name.setText(ctx.getStudent().first_name);
        student_last_name.setText(ctx.getStudent().last_name);
    }

    @Override
    public void onlineChanged(boolean on_line) {

    }
}
