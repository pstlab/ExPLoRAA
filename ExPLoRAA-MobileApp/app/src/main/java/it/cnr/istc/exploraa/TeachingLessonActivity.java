package it.cnr.istc.exploraa;

import android.os.Bundle;
import android.support.annotation.NonNull;
import android.support.v7.app.AppCompatActivity;
import android.support.v7.widget.LinearLayoutManager;
import android.support.v7.widget.RecyclerView;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import it.cnr.istc.exploraa.api.Lesson;
import it.cnr.istc.exploraa.api.LessonModel;

public class TeachingLessonActivity extends AppCompatActivity implements TeachingLessonContext.TeachingLessonListener {

    private TeachingLessonContext ctx;
    private Menu options_menu;
    private TextView teaching_lesson_name;
    private TextView teaching_lesson_time;
    private RecyclerView teaching_lesson_tokens_recycler_view;
    private TokensAdapter teaching_lesson_tokens_adapter;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_teaching_lesson);

        teaching_lesson_name = findViewById(R.id.teaching_lesson_name);
        teaching_lesson_time = findViewById(R.id.teaching_lesson_time);
        teaching_lesson_tokens_recycler_view = findViewById(R.id.teaching_lesson_tokens_recycler_view);

        long lesson_id = getIntent().getLongExtra("lesson_id", -1);
        ctx = ExPLoRAAContext.getInstance().getTeachingLesson(getIntent().getLongExtra("lesson_id", -1));

        teaching_lesson_name.setText(ctx.getLesson().name);

        teaching_lesson_tokens_adapter = new TokensAdapter(this);

        // use this setting to improve performance if you know that changes
        // in content do not change the layout size of the RecyclerView
        teaching_lesson_tokens_recycler_view.setHasFixedSize(true);
        teaching_lesson_tokens_recycler_view.setLayoutManager(new LinearLayoutManager(this));
        teaching_lesson_tokens_recycler_view.setAdapter(teaching_lesson_tokens_adapter);
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        MenuInflater inflater = getMenuInflater();
        inflater.inflate(R.menu.teaching_lesson_menu, menu);
        options_menu = menu;
        stateChanged(ctx.getLesson().state);
        return true;
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case R.id.teaching_lesson_play_menu_item:
                ExPLoRAAContext.getInstance().play(this, ctx.getLesson());
                return true;
            case R.id.teaching_lesson_pause_menu_item:
                ExPLoRAAContext.getInstance().pause(this, ctx.getLesson());
                return true;
            case R.id.teaching_lesson_stop_menu_item:
                ExPLoRAAContext.getInstance().stop(this, ctx.getLesson());
                return true;
            default:
                return super.onOptionsItemSelected(item);
        }
    }

    @Override
    public void onResume() {
        super.onResume();
        teaching_lesson_time.setText(ExPLoRAAContext.convertTimeToString(ctx.getLesson().time));
        ctx.addListener(this);
    }

    @Override
    public void onPause() {
        super.onPause();
        ctx.removeListener(this);
    }

    @Override
    public void timeChanged(long t) {
        teaching_lesson_time.setText(ExPLoRAAContext.convertTimeToString(t));
    }

    @Override
    public void stateChanged(Lesson.LessonState state) {
        switch (state) {
            case Running:
                options_menu.findItem(R.id.teaching_lesson_play_menu_item).setVisible(false);
                options_menu.findItem(R.id.teaching_lesson_pause_menu_item).setVisible(true);
                options_menu.findItem(R.id.teaching_lesson_stop_menu_item).setVisible(true);
                break;
            case Paused:
                options_menu.findItem(R.id.teaching_lesson_play_menu_item).setVisible(true);
                options_menu.findItem(R.id.teaching_lesson_pause_menu_item).setVisible(false);
                options_menu.findItem(R.id.teaching_lesson_stop_menu_item).setVisible(true);
                break;
            case Stopped:
                options_menu.findItem(R.id.teaching_lesson_play_menu_item).setVisible(true);
                options_menu.findItem(R.id.teaching_lesson_pause_menu_item).setVisible(false);
                options_menu.findItem(R.id.teaching_lesson_stop_menu_item).setVisible(false);
                break;
        }
    }

    @Override
    public void studentAdded(int pos, StudentContext s_ctx) {
    }

    @Override
    public void studentRemoved(int pos, StudentContext s_ctx) {
    }

    @Override
    public void addedToken(int pos, TeachingLessonContext.TokenRow tk) {
        teaching_lesson_tokens_adapter.notifyItemInserted(pos);
    }

    @Override
    public void removedToken(int pos, TeachingLessonContext.TokenRow tk) {
        teaching_lesson_tokens_adapter.notifyItemRemoved(pos);
    }

    @Override
    public void updatedToken(int pos, TeachingLessonContext.TokenRow tk) {
        teaching_lesson_tokens_adapter.notifyItemChanged(pos);
    }

    private static class TokensAdapter extends RecyclerView.Adapter<TokenView> {

        private TeachingLessonActivity activity;

        private TokensAdapter(TeachingLessonActivity activity) {
            this.activity = activity;
        }

        @NonNull
        @Override
        public TokenView onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
            return new TokenView(activity, LayoutInflater.from(parent.getContext()).inflate(R.layout.token_row, parent, false));
        }

        @Override
        public void onBindViewHolder(@NonNull TokenView holder, int position) {
            holder.setToken(activity.ctx.getTokens().get(position));
        }

        @Override
        public int getItemCount() {
            return activity.ctx.getTokens().size();
        }
    }

    private static class TokenView extends RecyclerView.ViewHolder {

        private TeachingLessonActivity activity;
        private TextView token_time;
        private TextView token_min;
        private TextView token_max;
        private TextView token_content;
        private TeachingLessonContext.TokenRow token;

        private TokenView(TeachingLessonActivity activity, View view) {
            super(view);
            this.activity = activity;
            token_time = view.findViewById(R.id.token_time);
            token_min = view.findViewById(R.id.token_min);
            token_max = view.findViewById(R.id.token_max);
            token_content = view.findViewById(R.id.token_content);
        }

        private void setToken(TeachingLessonContext.TokenRow token) {
            this.token = token;
            token_time.setText(ExPLoRAAContext.convertTimeToString(token.getTime()));
            token_min.setText(ExPLoRAAContext.convertTimeToString(token.getMin()));
            token_max.setText(ExPLoRAAContext.convertTimeToString(token.getMax()));
            final LessonModel.StimulusTemplate stimulus = activity.ctx.getLesson().model.stimuli.get(token.getToken().refEvent);
            switch (stimulus.type) {
                case Root:
                    break;
                case Text:
                    token_content.setText(((LessonModel.StimulusTemplate.TextStimulusTemplate) stimulus).content);
                    break;
                case URL:
                    token_content.setText(((LessonModel.StimulusTemplate.URLStimulusTemplate) stimulus).content);
                    break;
                case Question:
                    token_content.setText(((LessonModel.StimulusTemplate.QuestionStimulusTemplate) stimulus).question);
                    break;
            }
        }
    }
}
