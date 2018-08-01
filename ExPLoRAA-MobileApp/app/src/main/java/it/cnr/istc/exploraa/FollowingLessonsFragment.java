package it.cnr.istc.exploraa;

import android.content.Intent;
import android.os.Bundle;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.support.v4.app.Fragment;
import android.support.v7.widget.LinearLayoutManager;
import android.support.v7.widget.RecyclerView;
import android.view.GestureDetector;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import java.util.HashSet;
import java.util.Set;

public class FollowingLessonsFragment extends Fragment {

    private RecyclerView following_lessons_recycler_view;
    private FollowingLessonsAdapter following_lessons_adapter;

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setHasOptionsMenu(true);
    }

    @Override
    public void onCreateOptionsMenu(Menu menu, MenuInflater inflater) {
        inflater.inflate(R.menu.following_lessons_menu, menu);
        super.onCreateOptionsMenu(menu, inflater);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case R.id.add_following_lesson_menu_item:
                startActivity(new Intent(getActivity(), EnrollActivity.class));
                return true;
            default:
                return super.onOptionsItemSelected(item);
        }
    }

    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        // Inflate the layout for this fragment
        return inflater.inflate(R.layout.fragment_following_lessons, container, false);
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        following_lessons_recycler_view = view.findViewById(R.id.following_lessons_recycler_view);
        following_lessons_adapter = new FollowingLessonsAdapter(this);

        // use this setting to improve performance if you know that changes
        // in content do not change the layout size of the RecyclerView
        following_lessons_recycler_view.setHasFixedSize(true);
        following_lessons_recycler_view.setLayoutManager(new LinearLayoutManager(getContext()));
        following_lessons_recycler_view.setAdapter(following_lessons_adapter);
    }

    @Override
    public void onResume() {
        super.onResume();
        ExPLoRAAContext.getInstance().addFollowingLessonsListener(following_lessons_adapter);
    }

    @Override
    public void onPause() {
        super.onPause();
        ExPLoRAAContext.getInstance().removeFollowingLessonsListener(following_lessons_adapter);
    }

    private static class FollowingLessonsAdapter extends RecyclerView.Adapter<FollowingLessonView> implements ExPLoRAAContext.FollowingLessonsListener {

        private FollowingLessonsFragment frgmnt;
        private Set<Integer> selected_lessons = new HashSet<>();

        private FollowingLessonsAdapter(FollowingLessonsFragment frgmnt) {
            this.frgmnt = frgmnt;
        }

        @NonNull
        @Override
        public FollowingLessonView onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
            return new FollowingLessonView(frgmnt, LayoutInflater.from(parent.getContext()).inflate(R.layout.following_lesson_row, parent, false));
        }

        @Override
        public void onBindViewHolder(@NonNull FollowingLessonView holder, int position) {
            holder.setLesson(ExPLoRAAContext.getInstance().getFollowingLessons().get(position));
        }

        @Override
        public int getItemCount() {
            return ExPLoRAAContext.getInstance().getFollowingLessons().size();
        }

        @Override
        public void followingLessonAdded(int pos, FollowingLessonContext ctx) {
            notifyItemInserted(pos);
        }

        @Override
        public void followingLessonUpdated(int pos, FollowingLessonContext ctx) {
            notifyItemChanged(pos);
        }

        @Override
        public void followingLessonRemoved(int pos, FollowingLessonContext ctx) {
            notifyItemRemoved(pos);
        }

        @Override
        public void followingLessonsCleared() {
            notifyDataSetChanged();
        }
    }

    private static class FollowingLessonView extends RecyclerView.ViewHolder implements View.OnTouchListener {

        private final GestureDetector gestureDetector;
        private TextView title;
        private FollowingLessonContext lesson;

        private FollowingLessonView(final FollowingLessonsFragment frgmnt, View view) {
            super(view);
            view.setOnTouchListener(this);
            gestureDetector = new GestureDetector(frgmnt.getContext(), new GestureDetector.SimpleOnGestureListener() {
                public void onLongPress(MotionEvent e) {
                    int pos = getAdapterPosition();
                    if (frgmnt.following_lessons_adapter.selected_lessons.contains(pos)) {
                        frgmnt.following_lessons_adapter.selected_lessons.remove(pos);
                    } else {
                        frgmnt.following_lessons_adapter.selected_lessons.add(pos);
                    }
                    frgmnt.following_lessons_adapter.followingLessonUpdated(pos, lesson);
                }

                @Override
                public boolean onDown(MotionEvent e) {
                    if (frgmnt.following_lessons_adapter.selected_lessons.isEmpty()) {
                        // we show the teaching lesson's details..
                        final Intent intent = new Intent(frgmnt.getContext(), FollowingLessonActivity.class);
                        intent.putExtra("lesson", lesson);
                        frgmnt.startActivity(intent);
                    } else {
                        int pos = getAdapterPosition();
                        if (frgmnt.following_lessons_adapter.selected_lessons.contains(pos)) {
                            frgmnt.following_lessons_adapter.selected_lessons.remove(pos);
                        } else {
                            frgmnt.following_lessons_adapter.selected_lessons.add(pos);
                        }
                        frgmnt.following_lessons_adapter.followingLessonUpdated(pos, lesson);
                    }
                    return true;
                }
            });
            title = view.findViewById(R.id.following_lesson_name);
        }

        private void setLesson(FollowingLessonContext lesson) {
            this.lesson = lesson;
            title.setText(lesson.getLesson().name);
        }

        @Override
        public boolean onTouch(View v, MotionEvent event) {
            gestureDetector.onTouchEvent(event);
            return true;
        }
    }
}
