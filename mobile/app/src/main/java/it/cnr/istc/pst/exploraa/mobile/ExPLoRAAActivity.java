package it.cnr.istc.pst.exploraa.mobile;

import android.os.Bundle;
import android.view.Menu;
import android.view.MenuInflater;

import androidx.annotation.NonNull;
import androidx.appcompat.app.AppCompatActivity;
import androidx.core.app.NotificationManagerCompat;
import androidx.fragment.app.Fragment;
import androidx.viewpager2.adapter.FragmentStateAdapter;
import androidx.viewpager2.widget.ViewPager2;

public class ExPLoRAAActivity extends AppCompatActivity {

    private Menu main_menu;
    private ViewPager2 pager;
    private final StimuliFragment stimuli_fragment = new StimuliFragment();
    private final FollowingLessonsFragment following_lessons_fragment = new FollowingLessonsFragment();
    private final TeachingLessonsFragment teaching_lessons_fragment = new TeachingLessonsFragment();
    private final StudentsFragment students_fragment = new StudentsFragment();

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_exploraa);
        pager = findViewById(R.id.main_pager);

        pager.setAdapter(new FragmentStateAdapter(this) {
            @NonNull
            @Override
            public Fragment createFragment(int position) {
                switch (position) {
                    case 0:
                        return stimuli_fragment;
                    case 1:
                        return following_lessons_fragment;
                    case 2:
                        return teaching_lessons_fragment;
                    case 3:
                        return students_fragment;
                    default:
                        throw new AssertionError("Invalid position..");
                }
            }

            @Override
            public int getItemCount() {
                return 4;
            }
        });

        // we clear all the notifications..
        NotificationManagerCompat.from(this).cancelAll();
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        MenuInflater inflater = getMenuInflater();
        inflater.inflate(R.menu.exploraa_menu, menu);
        main_menu = menu;
        return true;
    }
}
