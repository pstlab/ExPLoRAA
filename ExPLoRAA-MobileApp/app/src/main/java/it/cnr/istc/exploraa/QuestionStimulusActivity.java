package it.cnr.istc.exploraa;

import android.content.Intent;
import android.os.Bundle;
import android.support.v7.app.AppCompatActivity;
import android.view.View;
import android.widget.Button;
import android.widget.LinearLayout;
import android.widget.RadioButton;
import android.widget.RadioGroup;
import android.widget.TextView;

public class QuestionStimulusActivity extends AppCompatActivity {

    private long lesson_id;
    private int question_id;
    private TextView question_stimulus_content;
    private RadioGroup question_stimulus_answers_group;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_question_stimulus);

        question_stimulus_content = findViewById(R.id.question_stimulus_content);
        question_stimulus_answers_group = findViewById(R.id.question_stimulus_answers_group);

        onNewIntent(getIntent());
    }

    @Override
    protected void onNewIntent(Intent intent) {
        lesson_id = intent.getLongExtra("lesson_id", -1);
        question_id = intent.getIntExtra("question_id", -1);
        question_stimulus_content.setText(intent.getStringExtra("question"));
        int i = 0;
        for (CharSequence answer : intent.getCharSequenceArrayListExtra("answers")) {
            final RadioButton button = new RadioButton(this);
            button.setId(i++);
            button.setText(answer);
            LinearLayout.LayoutParams params = new LinearLayout.LayoutParams(RadioGroup.LayoutParams.MATCH_PARENT, RadioGroup.LayoutParams.WRAP_CONTENT);
            button.setLayoutParams(params);
            if (intent.hasExtra("answer"))
                button.setEnabled(false);
            question_stimulus_answers_group.addView(button);
        }

        if (intent.hasExtra("answer")) {
            question_stimulus_answers_group.check(intent.getIntExtra("answer", -1));
            question_stimulus_answers_group.setEnabled(false);
        }

        final Button bt = new Button(this);
        bt.setEnabled(false);
        bt.setText(getString(R.string.send));
        bt.setLayoutParams(new LinearLayout.LayoutParams(RadioGroup.LayoutParams.MATCH_PARENT, RadioGroup.LayoutParams.WRAP_CONTENT));
        question_stimulus_answers_group.addView(bt);

        question_stimulus_answers_group.setOnCheckedChangeListener(new RadioGroup.OnCheckedChangeListener() {
            @Override
            public void onCheckedChanged(RadioGroup radioGroup, int i) {
                bt.setEnabled(true);
            }
        });

        bt.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                ExPLoRAAContext.getInstance().getService().answerQuestion(lesson_id, question_id, question_stimulus_answers_group.getCheckedRadioButtonId());
                finish();
            }
        });
    }
}
