package it.cnr.istc.exploraa;

import android.content.Intent;
import android.os.Bundle;
import android.support.annotation.NonNull;
import android.support.v7.app.AppCompatActivity;
import android.support.v7.widget.DividerItemDecoration;
import android.support.v7.widget.LinearLayoutManager;
import android.support.v7.widget.RecyclerView;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

import java.util.ArrayList;
import java.util.List;

import it.cnr.istc.exploraa.api.Lesson;
import it.cnr.istc.exploraa.api.Message;

public class FollowingLessonActivity extends AppCompatActivity implements FollowingLessonContext.FollowingLessonListener {

    private FollowingLessonContext ctx;
    private ImageView following_lesson_status_image_view;
    private TextView following_lesson_name;
    private TextView following_lesson_time;
    private final StimuliAdapter stimuli_adapter = new StimuliAdapter();

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_following_lesson);
        long lesson_id = getIntent().getLongExtra("lesson_id", -1);
        ctx = ExPLoRAAContext.getInstance().getService().getFollowingLesson(lesson_id);
        ctx.addListener(this);
        following_lesson_name.setText(ctx.getLesson().name);
        following_lesson_time.setText(ExPLoRAAService.convertTimeToString(ctx.getTime()));
        switch (ctx.getState()) {
            case Running:
                following_lesson_status_image_view.setImageResource(R.drawable.ic_play);
                break;
            case Paused:
                following_lesson_status_image_view.setImageResource(R.drawable.ic_pause);
                break;
            case Stopped:
                following_lesson_status_image_view.setImageResource(R.drawable.ic_stop);
                break;
        }
        stimuli_adapter.setStimuli(ctx.getStimuli());

        following_lesson_status_image_view = findViewById(R.id.activity_following_lesson_status_image_view);
        following_lesson_name = findViewById(R.id.activity_following_lesson_name);
        following_lesson_time = findViewById(R.id.activity_following_lesson_time);
        RecyclerView following_lesson_stimuli_recycler_view = findViewById(R.id.activity_following_lesson_stimuli_recycler_view);

        // use this setting to improve performance if you know that changes
        // in content do not change the layout size of the RecyclerView
        following_lesson_stimuli_recycler_view.setHasFixedSize(true);
        following_lesson_stimuli_recycler_view.setLayoutManager(new LinearLayoutManager(this));
        following_lesson_stimuli_recycler_view.setAdapter(stimuli_adapter);
        following_lesson_stimuli_recycler_view.addItemDecoration(new DividerItemDecoration(this, DividerItemDecoration.VERTICAL));
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        ctx.removeListener(this);
    }

    @Override
    public void timeChanged(long t) {
        following_lesson_time.setText(ExPLoRAAService.convertTimeToString(t));
    }

    @Override
    public void stateChanged(Lesson.LessonState state) {
        switch (state) {
            case Running:
                following_lesson_status_image_view.setImageResource(R.drawable.ic_play);
                break;
            case Paused:
                following_lesson_status_image_view.setImageResource(R.drawable.ic_pause);
                break;
            case Stopped:
                following_lesson_status_image_view.setImageResource(R.drawable.ic_stop);
                break;
        }
    }

    @Override
    public void addedStimulus(int pos, Message.Stimulus e) {
        stimuli_adapter.notifyItemInserted(pos);
    }

    @Override
    public void removedStimulus(int pos, Message.Stimulus e) {
        stimuli_adapter.notifyItemRemoved(pos);
    }

    private class StimuliAdapter extends RecyclerView.Adapter<StimulusView> {

        private List<Message.Stimulus> stimuli = new ArrayList<>();

        private void setStimuli(List<Message.Stimulus> stimuli) {
            this.stimuli = stimuli;
            notifyDataSetChanged();
        }

        @NonNull
        @Override
        public StimulusView onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
            return new StimulusView(LayoutInflater.from(parent.getContext()).inflate(R.layout.stimulus_row, parent, false));
        }

        @Override
        public void onBindViewHolder(@NonNull StimulusView holder, int position) {
            holder.setStimulus(stimuli.get(position));
        }

        @Override
        public int getItemCount() {
            return stimuli.size();
        }
    }

    private class StimulusView extends RecyclerView.ViewHolder implements View.OnClickListener {

        private TextView title;
        private Message.Stimulus stimulus;

        private StimulusView(View view) {
            super(view);
            view.setOnClickListener(this);
            title = view.findViewById(R.id.stimulus_title);
        }

        private void setStimulus(Message.Stimulus stimulus) {
            this.stimulus = stimulus;
            switch (stimulus.stimulus_type) {
                case Text:
                    title.setText(((Message.Stimulus.TextStimulus) stimulus).content);
                    break;
                case Question:
                    title.setText(((Message.Stimulus.QuestionStimulus) stimulus).question);
                    break;
                case URL:
                    title.setText(((Message.Stimulus.URLStimulus) stimulus).content);
                    break;
            }
        }

        @Override
        public void onClick(View v) {
            switch (stimulus.stimulus_type) {
                case Text:
                    final Intent text_intent = new Intent(FollowingLessonActivity.this, TextStimulusActivity.class);
                    text_intent.putExtra("content", ((Message.Stimulus.TextStimulus) stimulus).content);
                    startActivity(text_intent);
                    break;
                case Question:
                    final Intent question_intent = new Intent(FollowingLessonActivity.this, TextStimulusActivity.class);
                    question_intent.putExtra("question", ((Message.Stimulus.QuestionStimulus) stimulus).question);
                    ArrayList<CharSequence> answers = new ArrayList<>(((Message.Stimulus.QuestionStimulus) stimulus).answers.size());
                    answers.addAll(((Message.Stimulus.QuestionStimulus) stimulus).answers);
                    question_intent.putExtra("answers", answers);
                    if (((Message.Stimulus.QuestionStimulus) stimulus).answer != null)
                        question_intent.putExtra("answer", ((Message.Stimulus.QuestionStimulus) stimulus).answer);
                    startActivity(question_intent);
                    break;
                case URL:
                    final Intent url_intent = new Intent(FollowingLessonActivity.this, TextStimulusActivity.class);
                    url_intent.putExtra("content", ((Message.Stimulus.URLStimulus) stimulus).content);
                    url_intent.putExtra("url", ((Message.Stimulus.URLStimulus) stimulus).url);
                    startActivity(url_intent);
                    break;
            }
        }
    }
}
