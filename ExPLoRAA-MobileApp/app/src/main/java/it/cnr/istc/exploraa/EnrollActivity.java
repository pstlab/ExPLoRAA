package it.cnr.istc.exploraa;

import android.os.Bundle;
import android.support.annotation.NonNull;
import android.support.v7.app.AppCompatActivity;
import android.support.v7.widget.LinearLayoutManager;
import android.support.v7.widget.RecyclerView;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.ExecutionException;

import it.cnr.istc.exploraa.api.Lesson;

public class EnrollActivity extends AppCompatActivity {

    private static final String TAG = "EnrollActivity";
    private RecyclerView enrolling_lessons_recycler_view;
    private EnrollingLessonsAdapter enrolling_lessons_adapter;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_enroll);

        enrolling_lessons_recycler_view = findViewById(R.id.enrolling_lessons_recycler_view);
        try {
            enrolling_lessons_adapter = new EnrollingLessonsAdapter(ExPLoRAAContext.getInstance().getLessons(this));
        } catch (ExecutionException | InterruptedException e) {
            Log.w(TAG, "Lesson retrieval failed..", e);
        }

        // use this setting to improve performance if you know that changes
        // in content do not change the layout size of the RecyclerView
        enrolling_lessons_recycler_view.setHasFixedSize(true);
        enrolling_lessons_recycler_view.setLayoutManager(new LinearLayoutManager(this));
        enrolling_lessons_recycler_view.setAdapter(enrolling_lessons_adapter);
    }

    private static class EnrollingLessonsAdapter extends RecyclerView.Adapter<EnrollingLessonView> {

        final List<Lesson> lessons;

        private EnrollingLessonsAdapter(Collection<Lesson> lessons) {
            this.lessons = new ArrayList<>(lessons);
        }

        @NonNull
        @Override
        public EnrollingLessonView onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
            return new EnrollingLessonView(LayoutInflater.from(parent.getContext()).inflate(R.layout.enrolling_lesson_row, parent, false));
        }

        @Override
        public void onBindViewHolder(@NonNull EnrollingLessonView holder, int position) {
            holder.setLesson(lessons.get(position));
        }

        @Override
        public int getItemCount() {
            return lessons.size();
        }
    }

    private static class EnrollingLessonView extends RecyclerView.ViewHolder implements View.OnClickListener {

        private Lesson l;
        private TextView title;

        private EnrollingLessonView(View view) {
            super(view);
            view.setOnClickListener(this);
            title = view.findViewById(R.id.enrolling_lesson_name);
        }

        private void setLesson(Lesson l) {
            this.l = l;
            title.setText(l.name);
        }

        @Override
        public void onClick(View v) {
            Log.d("EnrollingLessonView", "onClick " + getAdapterPosition() + " " + title.getText());
        }
    }
}
