package it.cnr.istc.exploraa;

import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.support.v4.app.FragmentPagerAdapter;
import android.support.v4.view.ViewPager;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

public class TeachFragment extends Fragment {

    private ViewPager pager;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        // Inflate the layout for this fragment
        View v = inflater.inflate(R.layout.fragment_teach, container, false);

        pager = v.findViewById(R.id.teach_pager);

        pager.setAdapter(new FragmentPagerAdapter(getActivity().getSupportFragmentManager()) {
            @Override
            public Fragment getItem(int position) {
                switch (position) {
                    case 0:
                        return new LessonsFragment();
                    case 1:
                        return new StudentsFragment();
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
