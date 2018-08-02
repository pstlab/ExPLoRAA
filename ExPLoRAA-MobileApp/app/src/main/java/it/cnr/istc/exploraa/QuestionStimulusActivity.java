package it.cnr.istc.exploraa;

import android.os.Bundle;
import android.support.v7.app.AppCompatActivity;
import android.widget.LinearLayout;
import android.widget.RadioButton;
import android.widget.RadioGroup;
import android.widget.TextView;

public class QuestionStimulusActivity extends AppCompatActivity {

    private TextView question_stimulus_content;
    private RadioGroup question_stimulus_answers_group;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_question_stimulus);

        question_stimulus_content = findViewById(R.id.question_stimulus_content);
        question_stimulus_answers_group = findViewById(R.id.question_stimulus_answers_group);

        question_stimulus_content.setText(getIntent().getStringExtra("question"));
        int i = 0;
        for (CharSequence answer : getIntent().getCharSequenceArrayListExtra("answers")) {
            final RadioButton button = new RadioButton(this);
            button.setId(i++);
            button.setText(answer);
            LinearLayout.LayoutParams params = new LinearLayout.LayoutParams(RadioGroup.LayoutParams.MATCH_PARENT, RadioGroup.LayoutParams.WRAP_CONTENT);
            button.setLayoutParams(params);
            question_stimulus_answers_group.addView(button);
        }
    }
}
