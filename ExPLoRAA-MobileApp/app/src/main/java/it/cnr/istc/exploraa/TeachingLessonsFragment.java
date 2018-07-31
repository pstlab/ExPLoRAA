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

public class TeachingLessonsFragment extends Fragment {

    private RecyclerView teaching_lessons_recycler_view;
    private TeachingLessonsAdapter teaching_lessons_adapter;

    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        // Inflate the layout for this fragment
        return inflater.inflate(R.layout.fragment_teaching_lessons, container, false);
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        teaching_lessons_recycler_view = view.findViewById(R.id.teaching_lessons_recycler_view);
        teaching_lessons_adapter = new TeachingLessonsAdapter();

        // use this setting to improve performance if you know that changes
        // in content do not change the layout size of the RecyclerView
        teaching_lessons_recycler_view.setHasFixedSize(true);
        teaching_lessons_recycler_view.setLayoutManager(new LinearLayoutManager(getContext()));
        teaching_lessons_recycler_view.setAdapter(teaching_lessons_adapter);
    }

    @Override
    public void onResume() {
        super.onResume();
        ExPLoRAAContext.getInstance().addTeachingLessonsListener(teaching_lessons_adapter);
    }

    @Override
    public void onPause() {
        super.onPause();
        ExPLoRAAContext.getInstance().removeTeachingLessonsListener(teaching_lessons_adapter);
    }

    private static class TeachingLessonsAdapter extends RecyclerView.Adapter<TeachingLessonView> implements ExPLoRAAContext.TeachingLessonsListener {

        @NonNull
        @Override
        public TeachingLessonView onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
            return new TeachingLessonView(LayoutInflater.from(parent.getContext()).inflate(R.layout.teaching_lesson_row, parent));
        }

        @Override
        public void onBindViewHolder(@NonNull TeachingLessonView holder, int position) {
            TeachingLessonContext lesson = ExPLoRAAContext.getInstance().getTeachingLessons().get(position);
            holder.title.setText(lesson.getLesson().name);
        }

        @Override
        public int getItemCount() {
            return ExPLoRAAContext.getInstance().getTeachingLessons().size();
        }

        @Override
        public void teachingLessonAdded(int pos, TeachingLessonContext ctx) {
            notifyItemInserted(pos);
        }

        @Override
        public void teachingLessonUpdated(int pos, TeachingLessonContext ctx) {
            notifyItemChanged(pos);
        }

        @Override
        public void teachingLessonRemoved(int pos, TeachingLessonContext ctx) {
            notifyItemRemoved(pos);
        }

        @Override
        public void teachingLessonsCleared() {
            notifyDataSetChanged();
        }
    }

    private static class TeachingLessonView extends RecyclerView.ViewHolder {

        public TextView title;

        public TeachingLessonView(View view) {
            super(view);
            title = view.findViewById(R.id.teaching_lesson_name);
        }
    }
}
