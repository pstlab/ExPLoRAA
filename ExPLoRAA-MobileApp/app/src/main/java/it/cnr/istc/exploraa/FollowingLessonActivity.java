package it.cnr.istc.exploraa;

import android.os.Bundle;
import android.support.v7.app.AppCompatActivity;

import it.cnr.istc.exploraa.api.Lesson;
import it.cnr.istc.exploraa.api.Message;

public class FollowingLessonActivity extends AppCompatActivity implements FollowingLessonContext.FollowingLessonListener {

    private FollowingLessonContext ctx;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_following_lesson);
        ctx = (FollowingLessonContext) getIntent().getSerializableExtra("lesson");
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
    public void addedStimulus(int position, Message.Stimulus e) {
    }

    @Override
    public void removedStimulus(int position, Message.Stimulus e) {
    }
}
