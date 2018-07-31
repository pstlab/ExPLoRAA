package it.cnr.istc.exploraa;

import android.os.Bundle;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.support.v4.app.Fragment;
import android.support.v7.widget.LinearLayoutManager;
import android.support.v7.widget.RecyclerView;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

public class FollowingLessonsFragment extends Fragment {

    private RecyclerView following_lessons_recycler_view;
    private FollowingLessonsAdapter following_lessons_adapter;

    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        // Inflate the layout for this fragment
        return inflater.inflate(R.layout.fragment_following_lessons, container, false);
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        following_lessons_recycler_view = view.findViewById(R.id.following_lessons_recycler_view);
        following_lessons_adapter = new FollowingLessonsAdapter();

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

        @NonNull
        @Override
        public FollowingLessonView onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
            return new FollowingLessonView(LayoutInflater.from(parent.getContext()).inflate(R.layout.following_lesson_row, parent));
        }

        @Override
        public void onBindViewHolder(@NonNull FollowingLessonView holder, int position) {
            TeachingLessonContext lesson = ExPLoRAAContext.getInstance().getTeachingLessons().get(position);
            holder.title.setText(lesson.getLesson().name);
        }

        @Override
        public int getItemCount() {
            return ExPLoRAAContext.getInstance().getTeachingLessons().size();
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

    private static class FollowingLessonView extends RecyclerView.ViewHolder {

        public TextView title;

        private FollowingLessonView(View view) {
            super(view);
            title = view.findViewById(R.id.following_lesson_name);
        }
    }
}
