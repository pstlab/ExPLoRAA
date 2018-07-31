package it.cnr.istc.exploraa;

import android.os.Bundle;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.support.v4.app.Fragment;
import android.support.v4.app.FragmentPagerAdapter;
import android.support.v4.view.ViewPager;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

public class LearnFragment extends Fragment {

    private ViewPager pager;

    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        // Inflate the layout for this fragment
        View v = inflater.inflate(R.layout.fragment_learn, container, false);

        pager = v.findViewById(R.id.learn_pager);

        pager.setAdapter(new FragmentPagerAdapter(getActivity().getSupportFragmentManager()) {
            @Override
            public Fragment getItem(int position) {
                switch (position) {
                    case 0:
                        return new StimuliFragment();
                    case 1:
                        return new FollowingLessonsFragment();
                    default:
                        throw new AssertionError("Invalid position..");
                }
            }

            @Override
            public int getCount() {
                return 2;
            }
        });

        return v;
    }
}
