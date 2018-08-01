package it.cnr.istc.exploraa;

import android.os.Bundle;
import android.support.v7.app.AppCompatActivity;

import it.cnr.istc.exploraa.api.Lesson;

public class TeachingLessonActivity extends AppCompatActivity implements TeachingLessonContext.TeachingLessonListener {

    private TeachingLessonContext ctx;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_teaching_lesson);
        long lesson_id = getIntent().getLongExtra("lesson_id", -1);
        ctx = ExPLoRAAContext.getInstance().getTeachingLesson(getIntent().getLongExtra("lesson_id", -1));
    }

    @Override
    public void onResume() {
        super.onResume();
        ctx.addListener(this);
    }

    @Override
    public void onPause() {
        super.onPause();
        ctx.removeListener(this);
    }

    @Override
    public void timeChanged(long t) {
    }

    @Override
    public void stateChanged(Lesson.LessonState state) {
    }

    @Override
    public void addedToken(TeachingLessonContext.TokenRow tk) {
    }

    @Override
    public void removedToken(TeachingLessonContext.TokenRow tk) {
    }
}
