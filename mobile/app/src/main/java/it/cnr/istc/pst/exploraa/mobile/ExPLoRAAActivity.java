package it.cnr.istc.pst.exploraa.mobile;

import android.Manifest;
import android.app.Activity;
import android.app.AlertDialog;
import android.app.Dialog;
import android.content.Context;
import android.content.pm.PackageManager;
import android.os.Bundle;
import android.util.Log;
import android.view.Menu;
import android.view.MenuInflater;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import androidx.core.app.ActivityCompat;
import androidx.core.app.NotificationManagerCompat;
import androidx.core.content.ContextCompat;
import androidx.fragment.app.DialogFragment;
import androidx.fragment.app.Fragment;
import androidx.viewpager2.adapter.FragmentStateAdapter;
import androidx.viewpager2.widget.ViewPager2;

public class ExPLoRAAActivity extends AppCompatActivity {

    public static final int ACCESS_FINE_LOCATION_REQUEST_CODE_ASK_PERMISSIONS = 123;
    private final StimuliFragment stimuli_fragment = new StimuliFragment();
    private final FollowingLessonsFragment following_lessons_fragment = new FollowingLessonsFragment();
    private final TeachingLessonsFragment teaching_lessons_fragment = new TeachingLessonsFragment();
    private final StudentsFragment students_fragment = new StudentsFragment();
    private Menu main_menu;
    private ViewPager2 pager;

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

        if (ContextCompat.checkSelfPermission(this, Manifest.permission.ACCESS_FINE_LOCATION) == PackageManager.PERMISSION_GRANTED)
            activateLocalization();
        else
            ActivityCompat.requestPermissions(this, new String[]{Manifest.permission.ACCESS_FINE_LOCATION}, ACCESS_FINE_LOCATION_REQUEST_CODE_ASK_PERMISSIONS);
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        MenuInflater inflater = getMenuInflater();
        inflater.inflate(R.menu.exploraa_menu, menu);
        main_menu = menu;
        return true;
    }

    @Override
    public void onRequestPermissionsResult(int requestCode, @NonNull String[] permissions, @NonNull int[] grantResults) {
        switch (requestCode) {
            case ACCESS_FINE_LOCATION_REQUEST_CODE_ASK_PERMISSIONS:
                for (int i = 0, len = permissions.length; i < len; i++) {
                    if (permissions[i].equals(Manifest.permission.ACCESS_FINE_LOCATION)) {
                        switch (grantResults[i]) {
                            case PackageManager.PERMISSION_GRANTED:
                                activateLocalization();
                                break;
                            case PackageManager.PERMISSION_DENIED:
                                if (shouldShowRequestPermissionRationale(Manifest.permission.ACCESS_FINE_LOCATION))
                                    new LocationRationale().show(getSupportFragmentManager(), "location_rationale");
                                else noLocalization();
                                break;
                        }
                    }
                }
        }
    }

    private void activateLocalization() {
        Log.i(ExPLoRAAActivity.class.getName(), "Activating localization..");
    }

    private void noLocalization() {
        Log.i(ExPLoRAAActivity.class.getName(), "Localization will not be used..");
    }

    public static class LocationRationale extends DialogFragment {

        private Activity activity;

        @Override
        public void onAttach(@NonNull Context context) {
            super.onAttach(context);
            activity = (Activity) context;
        }

        @NonNull
        @Override
        public Dialog onCreateDialog(@Nullable Bundle savedInstanceState) {
            return new AlertDialog.Builder(getActivity()).setMessage(R.string.location_rationale).setPositiveButton(R.string.ok, (dialog, id) -> ActivityCompat.requestPermissions(activity, new String[]{Manifest.permission.ACCESS_FINE_LOCATION}, ACCESS_FINE_LOCATION_REQUEST_CODE_ASK_PERMISSIONS)).create();
        }
    }
}
