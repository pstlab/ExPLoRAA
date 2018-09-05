package it.cnr.istc.exploraa;

import android.content.Intent;
import android.content.SharedPreferences;
import android.os.Bundle;
import android.preference.PreferenceManager;
import android.support.v4.app.Fragment;
import android.support.v4.app.FragmentPagerAdapter;
import android.support.v4.app.NotificationManagerCompat;
import android.support.v4.view.ViewPager;
import android.support.v7.app.AppCompatActivity;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;

public class MainActivity extends AppCompatActivity implements ExPLoRAAContext.ServiceListener {

    private static final String TAG = "MainActivity";
    private ViewPager pager;
    private final StimuliFragment stimuli_fragment = new StimuliFragment();
    private final FollowingLessonsFragment following_lessons_fragment = new FollowingLessonsFragment();
    private final TeachingLessonsFragment teaching_lessons_fragment = new TeachingLessonsFragment();
    private final StudentsFragment students_fragment = new StudentsFragment();

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
            public int getCount() {
                return 4;
            }
        });

        if (BuildConfig.DEBUG && !ExPLoRAAContext.getInstance().isServiceRunning())
            throw new RuntimeException("Service should be running..");
        final ExPLoRAAService service = ExPLoRAAContext.getInstance().getService();

        stimuli_fragment.setStimuli(service.getStimuli());
        following_lessons_fragment.setLessons(service.getFollowingLessons());
        teaching_lessons_fragment.setLessons(service.getTeachingLessons());
        students_fragment.setStudents(service.getStudents());

        // we clear all the notifications..
        NotificationManagerCompat.from(this).cancelAll();

        ExPLoRAAContext.getInstance().addServiceListener(this);
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();

        ExPLoRAAContext.getInstance().removeServiceListener(this);
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        MenuInflater inflater = getMenuInflater();
        inflater.inflate(R.menu.main_menu, menu);
        return true;
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case R.id.logout_menu_item:
                if (BuildConfig.DEBUG && !ExPLoRAAContext.getInstance().isServiceRunning())
                    throw new RuntimeException("Service should be running..");
                ExPLoRAAContext.getInstance().getService().logout();
                SharedPreferences shared_prefs = PreferenceManager.getDefaultSharedPreferences(this);
                SharedPreferences.Editor prefs_edit = shared_prefs.edit();
                prefs_edit.remove(getString(R.string.email));
                prefs_edit.remove(getString(R.string.password));
                prefs_edit.apply();

                startActivity(new Intent(this, LoginActivity.class));
                finish();

                ExPLoRAAContext.getInstance().stopService(this);
                return true;
            default:
                return super.onOptionsItemSelected(item);
        }
    }

    @Override
    public void serviceConnected(ExPLoRAAService service) {
    }

    @Override
    public void serviceDisonnected() {
        startActivity(new Intent(this, NavigatorActivity.class));
        finish();
    }
}
