package it.cnr.istc.exploraa;

import android.content.Intent;
import android.os.Bundle;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.support.v4.app.Fragment;
import android.support.v7.widget.LinearLayoutManager;
import android.support.v7.widget.RecyclerView;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

public class TeachingLessonsFragment extends Fragment {

    private RecyclerView teaching_lessons_recycler_view;
    private TeachingLessonsAdapter teaching_lessons_adapter;

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setHasOptionsMenu(true);
    }

    @Override
    public void onCreateOptionsMenu(Menu menu, MenuInflater inflater) {
        inflater.inflate(R.menu.teaching_lessons_menu, menu);
        super.onCreateOptionsMenu(menu, inflater);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case R.id.add_teaching_lesson_menu_item:
                startActivity(new Intent(getActivity(), NewLessonActivity.class));
                return true;
            default:
                return super.onOptionsItemSelected(item);
        }
    }

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
            return new TeachingLessonView(LayoutInflater.from(parent.getContext()).inflate(R.layout.teaching_lesson_row, parent, false));
        }

        @Override
        public void onBindViewHolder(@NonNull TeachingLessonView holder, int position) {
            holder.setLesson(ExPLoRAAContext.getInstance().getTeachingLessons().get(position));
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

    private static class TeachingLessonView extends RecyclerView.ViewHolder implements View.OnClickListener {

        private TeachingLessonContext lesson;
        private TextView title;

        private TeachingLessonView(View view) {
            super(view);
            view.setOnClickListener(this);
            title = view.findViewById(R.id.teaching_lesson_name);
        }

        private void setLesson(TeachingLessonContext lesson) {
            this.lesson = lesson;
            title.setText(lesson.getLesson().name);
        }

        @Override
        public void onClick(View v) {
            Log.d("TeachingLessonView", "onClick " + getAdapterPosition() + " " + title.getText());
        }
    }
}
