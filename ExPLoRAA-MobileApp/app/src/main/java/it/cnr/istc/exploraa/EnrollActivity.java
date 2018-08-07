package it.cnr.istc.exploraa;

import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.ServiceConnection;
import android.os.Bundle;
import android.os.IBinder;
import android.support.annotation.NonNull;
import android.support.v7.app.AppCompatActivity;
import android.support.v7.widget.DividerItemDecoration;
import android.support.v7.widget.LinearLayoutManager;
import android.support.v7.widget.RecyclerView;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;
import android.widget.Toast;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import it.cnr.istc.exploraa.api.Lesson;
import retrofit2.Call;
import retrofit2.Callback;
import retrofit2.Response;

public class EnrollActivity extends AppCompatActivity {

    private static final String TAG = "EnrollActivity";
    private static final int SELECT_TOPICS_REQUEST_CODE = 42;
    private RecyclerView enrolling_lessons_recycler_view;
    private EnrollingLessonsAdapter enrolling_lessons_adapter;
    private Lesson chosen_lesson;
    private ExPLoRAAService service;
    private ServiceConnection service_connection = new ServiceConnection() {
        @Override
        public void onServiceConnected(ComponentName name, IBinder binder) {
            service = ((ExPLoRAAService.ExPLoRAABinder) binder).getService();

            service.get_lessons(new Callback<Collection<Lesson>>() {
                @Override
                public void onResponse(Call<Collection<Lesson>> call, Response<Collection<Lesson>> response) {
                    if (!response.isSuccessful()) try {
                        Toast.makeText(EnrollActivity.this, response.errorBody().string(), Toast.LENGTH_SHORT).show();
                    } catch (IOException e) {
                        e.printStackTrace();
                    }
                    else enrolling_lessons_adapter.setLessons(response.body());
                }

                @Override
                public void onFailure(Call<Collection<Lesson>> call, Throwable t) {
                    Log.e(TAG, null, t);
                }
            });
        }

        @Override
        public void onServiceDisconnected(ComponentName name) {
            service = null;
        }
    };

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_enroll);

        enrolling_lessons_recycler_view = findViewById(R.id.enrolling_lessons_recycler_view);
        enrolling_lessons_adapter = new EnrollingLessonsAdapter();

        // use this setting to improve performance if you know that changes
        // in content do not change the layout size of the RecyclerView
        enrolling_lessons_recycler_view.setHasFixedSize(true);
        enrolling_lessons_recycler_view.setLayoutManager(new LinearLayoutManager(this));
        enrolling_lessons_recycler_view.setAdapter(enrolling_lessons_adapter);
        enrolling_lessons_recycler_view.addItemDecoration(new DividerItemDecoration(this, DividerItemDecoration.VERTICAL));

        // we bind the ExPLoRAA service..
        if (!bindService(new Intent(this, ExPLoRAAService.class), service_connection, Context.BIND_AUTO_CREATE))
            Log.e(TAG, "Error: The requested service doesn't exist, or this client isn't allowed access to it.");
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        if (service_connection != null) {
            unbindService(service_connection);
        }
    }

    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
        if (requestCode == SELECT_TOPICS_REQUEST_CODE) {
            if (resultCode == RESULT_OK) {
                final ArrayList<CharSequence> topics = data.getCharSequenceArrayListExtra("topics");
                service.follow_lesson(chosen_lesson, topics);
                finish();
            }
        }
    }

    private class EnrollingLessonsAdapter extends RecyclerView.Adapter<EnrollingLessonView> {

        private final List<Lesson> lessons = new ArrayList<>();

        private void setLessons(@NonNull Collection<Lesson> lessons) {
            this.lessons.clear();
            this.lessons.addAll(lessons);
            notifyDataSetChanged();
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

    private class EnrollingLessonView extends RecyclerView.ViewHolder implements View.OnClickListener {

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
            chosen_lesson = l;
            ArrayList<CharSequence> topics = new ArrayList<>(l.topics.size());
            topics.addAll(l.topics);
            final Intent intent = new Intent(EnrollActivity.this, TopicsActivity.class);
            intent.putCharSequenceArrayListExtra("topics", topics);
            startActivityForResult(intent, SELECT_TOPICS_REQUEST_CODE);
        }
    }
}
