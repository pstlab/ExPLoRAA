package it.cnr.istc.exploraa;

import android.bluetooth.BluetoothAdapter;
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

public class MainActivity extends AppCompatActivity implements ExPLoRAAService.EmpaticaE4Listener {

    private static final String TAG = "MainActivity";
    private static final int REQUEST_ENABLE_BT = 42;
    private Menu main_menu;
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

        if (ExPLoRAAContext.getInstance().getService().getUser() == null) {
            SharedPreferences shared_prefs = PreferenceManager.getDefaultSharedPreferences(this);
            if (shared_prefs.contains(getString(R.string.email)) && shared_prefs.contains(getString(R.string.password)))
                ExPLoRAAContext.getInstance().getService().login(shared_prefs.getString(getString(R.string.email), null), shared_prefs.getString(getString(R.string.password), null));
        }

        service.addEmpaticaE4Listener(this);
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();

        if (ExPLoRAAContext.getInstance().isServiceRunning())
            ExPLoRAAContext.getInstance().getService().removeEmpaticaE4Listener(this);
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        MenuInflater inflater = getMenuInflater();
        inflater.inflate(R.menu.main_menu, menu);
        main_menu = menu;
        if (ExPLoRAAContext.getInstance().getService().isEmpaticaE4Connected()) {
            main_menu.findItem(R.id.connect_empatica_e4_menu_item).setVisible(false);
            main_menu.findItem(R.id.disconnect_empatica_e4_menu_item).setVisible(true);
        } else {
            main_menu.findItem(R.id.connect_empatica_e4_menu_item).setVisible(true);
            main_menu.findItem(R.id.disconnect_empatica_e4_menu_item).setVisible(false);
        }
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

                ExPLoRAAContext.getInstance().stopService(this);

                startActivity(new Intent(this, LoginActivity.class));
                finish();
                return true;
            case R.id.connect_empatica_e4_menu_item:
                BluetoothAdapter bluetooth_adapter = BluetoothAdapter.getDefaultAdapter();
                if (bluetooth_adapter != null) {
                    if (BuildConfig.DEBUG && !ExPLoRAAContext.getInstance().isServiceRunning())
                        throw new RuntimeException("Service should be running..");
                    if (bluetooth_adapter.isEnabled())
                        ExPLoRAAContext.getInstance().getService().connectEmpaticaE4();
                    else {
                        Intent enableBtIntent = new Intent(BluetoothAdapter.ACTION_REQUEST_ENABLE);
                        startActivityForResult(enableBtIntent, REQUEST_ENABLE_BT);
                    }
                }
                return true;
            case R.id.disconnect_empatica_e4_menu_item:
                if (BuildConfig.DEBUG && !ExPLoRAAContext.getInstance().isServiceRunning())
                    throw new RuntimeException("Service should be running..");
                ExPLoRAAContext.getInstance().getService().disconnectEmpaticaE4();
                return true;
            default:
                return super.onOptionsItemSelected(item);
        }
    }

    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
        super.onActivityResult(requestCode, resultCode, data);
        if (requestCode == REQUEST_ENABLE_BT && resultCode == RESULT_OK) {
            ExPLoRAAContext.getInstance().getService().connectEmpaticaE4();
        }
    }

    @Override
    public void empaticaE4Connected() {
        main_menu.findItem(R.id.connect_empatica_e4_menu_item).setVisible(false);
        main_menu.findItem(R.id.disconnect_empatica_e4_menu_item).setVisible(true);
    }

    @Override
    public void empaticaE4Disconnected() {
        main_menu.findItem(R.id.connect_empatica_e4_menu_item).setVisible(true);
        main_menu.findItem(R.id.disconnect_empatica_e4_menu_item).setVisible(false);
    }
}
