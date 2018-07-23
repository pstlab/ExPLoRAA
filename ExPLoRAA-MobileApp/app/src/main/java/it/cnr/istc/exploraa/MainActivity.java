package it.cnr.istc.exploraa;

import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.support.v4.app.FragmentPagerAdapter;
import android.support.v4.view.ViewPager;
import android.support.v7.app.AppCompatActivity;

public class MainActivity extends AppCompatActivity implements ContextListener {

    private ViewPager pager;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        pager = findViewById(R.id.main_pager);

        pager.setAdapter(new FragmentPagerAdapter(getSupportFragmentManager()) {
            @Override
            public Fragment getItem(int position) {
                switch (position) {
                    case 0:
                        return new LearnFragment();
                    case 1:
                        return new TeachFragment();
                    default:
                        throw new AssertionError("Invalid position..");
                }
            }

            @Override
            public int getCount() {
                return 2;
            }
        });
    }

    @Override
    public void onResume() {
        super.onResume();
        ExPLoRAAContext.getInstance().addListener(this);
    }

    @Override
    public void onPause() {
        super.onPause();
        ExPLoRAAContext.getInstance().removeListener(this);
    }

    @Override
    public void teacherAdded(TeacherContext ctx) {
    }

    @Override
    public void teacherRemoved(TeacherContext t) {
    }

    @Override
    public void teachersCleared() {
    }

    @Override
    public void followingLessonAdded(FollowingLessonContext ctx) {
    }

    @Override
    public void followingLessonRemoved(FollowingLessonContext l) {
    }

    @Override
    public void followingLessonsCleared() {
    }

    @Override
    public void teachingLessonAdded(TeachingLessonContext ctx) {
    }

    @Override
    public void teachingLessonRemoved(TeachingLessonContext ctx) {
    }

    @Override
    public void teachingLessonsCleared() {
    }

    @Override
    public void studentAdded(StudentContext ctx) {
    }

    @Override
    public void studentRemoved(StudentContext ctx) {
    }

    @Override
    public void studentsCleared() {
    }
}
