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

public class TeachersFragment extends Fragment {

    private RecyclerView teachers_recycler_view;
    private TeachersAdapter teachers_adapter;

    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        // Inflate the layout for this fragment
        return inflater.inflate(R.layout.fragment_teachers, container, false);
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        teachers_recycler_view = view.findViewById(R.id.teaching_lessons_recycler_view);
        teachers_adapter = new TeachersAdapter();

        // use this setting to improve performance if you know that changes
        // in content do not change the layout size of the RecyclerView
        teachers_recycler_view.setHasFixedSize(true);
        teachers_recycler_view.setLayoutManager(new LinearLayoutManager(getContext()));
        teachers_recycler_view.setAdapter(teachers_adapter);
    }

    @Override
    public void onResume() {
        super.onResume();
        ExPLoRAAContext.getInstance().addTeachersListener(teachers_adapter);
    }

    @Override
    public void onPause() {
        super.onPause();
        ExPLoRAAContext.getInstance().removeTeachersListener(teachers_adapter);
    }

    private static class TeachersAdapter extends RecyclerView.Adapter<TeacherView> implements ExPLoRAAContext.TeachersListener {

        @NonNull
        @Override
        public TeacherView onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
            return new TeacherView(LayoutInflater.from(parent.getContext()).inflate(R.layout.teacher_row, parent));
        }

        @Override
        public void onBindViewHolder(@NonNull TeacherView holder, int position) {
            TeachingLessonContext lesson = ExPLoRAAContext.getInstance().getTeachingLessons().get(position);
            holder.title.setText(lesson.getLesson().name);
        }

        @Override
        public int getItemCount() {
            return ExPLoRAAContext.getInstance().getTeachingLessons().size();
        }

        @Override
        public void teacherAdded(int pos, TeacherContext ctx) {
            notifyItemInserted(pos);
        }

        @Override
        public void teacherUpdated(int pos, TeacherContext ctx) {
            notifyItemChanged(pos);
        }

        @Override
        public void teacherRemoved(int pos, TeacherContext ctx) {
            notifyItemRemoved(pos);
        }

        @Override
        public void teachersCleared() {
            notifyDataSetChanged();
        }
    }

    private static class TeacherView extends RecyclerView.ViewHolder {

        public TextView title;

        public TeacherView(View view) {
            super(view);
            title = view.findViewById(R.id.teacher_name);
        }
    }
}
