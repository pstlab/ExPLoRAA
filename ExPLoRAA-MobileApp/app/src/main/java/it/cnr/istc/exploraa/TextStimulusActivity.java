package it.cnr.istc.exploraa;

import android.os.Bundle;
import android.support.v7.app.AppCompatActivity;
import android.widget.TextView;

public class TextStimulusActivity extends AppCompatActivity {

    private TextView text_stimulus_content;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_text_stimulus);

        text_stimulus_content = findViewById(R.id.text_stimulus_content);

        text_stimulus_content.setText(getIntent().getStringExtra("content"));
    }
}
