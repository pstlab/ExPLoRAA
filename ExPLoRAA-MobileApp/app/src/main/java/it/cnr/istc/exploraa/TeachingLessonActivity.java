package it.cnr.istc.exploraa;

import android.os.Bundle;
import android.support.annotation.NonNull;
import android.support.v7.app.AppCompatActivity;
import android.support.v7.util.SortedList;
import android.support.v7.widget.DividerItemDecoration;
import android.support.v7.widget.LinearLayoutManager;
import android.support.v7.widget.RecyclerView;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

import it.cnr.istc.exploraa.api.Lesson;
import it.cnr.istc.exploraa.api.LessonModel;

public class TeachingLessonActivity extends AppCompatActivity implements TeachingLessonContext.TeachingLessonListener {

    private TeachingLessonContext ctx;
    private Menu options_menu;
    private ImageView teaching_lesson_status_image_view;
    private TextView teaching_lesson_time;
    private final TokensAdapter teaching_lesson_tokens_adapter = new TokensAdapter();

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_teaching_lesson);

        teaching_lesson_status_image_view = findViewById(R.id.activity_teaching_lesson_status_image_view);
        TextView teaching_lesson_name = findViewById(R.id.activity_teaching_lesson_name);
        teaching_lesson_time = findViewById(R.id.activity_teaching_lesson_time);
        RecyclerView teaching_lesson_tokens_recycler_view = findViewById(R.id.activity_teaching_lesson_tokens_recycler_view);

        long lesson_id = getIntent().getLongExtra("lesson_id", -1);
        ctx = ExPLoRAAContext.getInstance().getService().getTeachingLesson(lesson_id);
        ctx.addListener(this);
        teaching_lesson_name.setText(ctx.getLesson().name);
        teaching_lesson_time.setText(ExPLoRAAService.convertTimeToString(ctx.getTime()));
        switch (ctx.getState()) {
            case Running:
                teaching_lesson_status_image_view.setImageResource(R.drawable.ic_play);
                break;
            case Paused:
                teaching_lesson_status_image_view.setImageResource(R.drawable.ic_pause);
                break;
            case Stopped:
                teaching_lesson_status_image_view.setImageResource(R.drawable.ic_stop);
                break;
        }
        teaching_lesson_tokens_adapter.tokens.beginBatchedUpdates();
        teaching_lesson_tokens_adapter.tokens.addAll(ctx.getTokens());
        teaching_lesson_tokens_adapter.tokens.endBatchedUpdates();

        // use this setting to improve performance if you know that changes
        // in content do not change the layout size of the RecyclerView
        teaching_lesson_tokens_recycler_view.setHasFixedSize(true);
        teaching_lesson_tokens_recycler_view.setLayoutManager(new LinearLayoutManager(this));
        teaching_lesson_tokens_recycler_view.setAdapter(teaching_lesson_tokens_adapter);
        teaching_lesson_tokens_recycler_view.addItemDecoration(new DividerItemDecoration(this, DividerItemDecoration.VERTICAL));
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        ctx.removeListener(this);
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        MenuInflater inflater = getMenuInflater();
        inflater.inflate(R.menu.teaching_lesson_menu, menu);
        options_menu = menu;
        switch (ctx.getState()) {
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
        return true;
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case R.id.teaching_lesson_play_menu_item:
                ExPLoRAAContext.getInstance().getService().play(ctx.getLesson());
                return true;
            case R.id.teaching_lesson_pause_menu_item:
                ExPLoRAAContext.getInstance().getService().pause(ctx.getLesson());
                return true;
            case R.id.teaching_lesson_stop_menu_item:
                ExPLoRAAContext.getInstance().getService().stop(ctx.getLesson());
                return true;
            default:
                return super.onOptionsItemSelected(item);
        }
    }

    @Override
    public void timeChanged(long t) {
        teaching_lesson_time.setText(ExPLoRAAService.convertTimeToString(t));
    }

    @Override
    public void stateChanged(Lesson.LessonState state) {
        switch (state) {
            case Running:
                teaching_lesson_status_image_view.setImageResource(R.drawable.ic_play);
                options_menu.findItem(R.id.teaching_lesson_play_menu_item).setVisible(false);
                options_menu.findItem(R.id.teaching_lesson_pause_menu_item).setVisible(true);
                options_menu.findItem(R.id.teaching_lesson_stop_menu_item).setVisible(true);
                break;
            case Paused:
                teaching_lesson_status_image_view.setImageResource(R.drawable.ic_pause);
                options_menu.findItem(R.id.teaching_lesson_play_menu_item).setVisible(true);
                options_menu.findItem(R.id.teaching_lesson_pause_menu_item).setVisible(false);
                options_menu.findItem(R.id.teaching_lesson_stop_menu_item).setVisible(true);
                break;
            case Stopped:
                teaching_lesson_status_image_view.setImageResource(R.drawable.ic_stop);
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
        teaching_lesson_tokens_adapter.tokens.add(tk);
    }

    @Override
    public void removedToken(int pos, TeachingLessonContext.TokenRow tk) {
        teaching_lesson_tokens_adapter.tokens.remove(tk);
    }

    @Override
    public void updatedToken(int pos, TeachingLessonContext.TokenRow tk) {
        teaching_lesson_tokens_adapter.tokens.recalculatePositionOfItemAt(teaching_lesson_tokens_adapter.tokens.indexOf(tk));
    }

    private class TokensAdapter extends RecyclerView.Adapter<TokenView> {

        private SortedList<TeachingLessonContext.TokenRow> tokens;

        TokensAdapter() {
            tokens = new SortedList<>(TeachingLessonContext.TokenRow.class, new SortedList.Callback<TeachingLessonContext.TokenRow>() {
                @Override
                public int compare(TeachingLessonContext.TokenRow o1, TeachingLessonContext.TokenRow o2) {
                    return Long.compare(o1.getTime(), o2.getTime());
                }

                @Override
                public void onChanged(int position, int count) {
                    notifyItemRangeChanged(position, count);
                }

                @Override
                public boolean areContentsTheSame(TeachingLessonContext.TokenRow oldItem, TeachingLessonContext.TokenRow newItem) {
                    return false;
                }

                @Override
                public boolean areItemsTheSame(TeachingLessonContext.TokenRow item1, TeachingLessonContext.TokenRow item2) {
                    return false;
                }

                @Override
                public void onInserted(int position, int count) {
                    notifyItemRangeInserted(position, count);
                }

                @Override
                public void onRemoved(int position, int count) {
                    notifyItemRangeRemoved(position, count);
                }

                @Override
                public void onMoved(int fromPosition, int toPosition) {
                    notifyItemMoved(fromPosition, toPosition);
                }
            });
        }

        @NonNull
        @Override
        public TokenView onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
            return new TokenView(LayoutInflater.from(parent.getContext()).inflate(R.layout.token_row, parent, false));
        }

        @Override
        public void onBindViewHolder(@NonNull TokenView holder, int position) {
            holder.setToken(tokens.get(position));
        }

        @Override
        public int getItemCount() {
            return tokens.size();
        }
    }

    private class TokenView extends RecyclerView.ViewHolder {

        private TextView token_time;
        private TextView token_min;
        private TextView token_max;
        private TextView token_content;

        private TokenView(View view) {
            super(view);
            token_time = view.findViewById(R.id.token_time);
            token_min = view.findViewById(R.id.token_min);
            token_max = view.findViewById(R.id.token_max);
            token_content = view.findViewById(R.id.token_content);
        }

        private void setToken(TeachingLessonContext.TokenRow token) {
            token_time.setText(ExPLoRAAService.convertTimeToString(token.getTime()));
            token_min.setText(ExPLoRAAService.convertTimeToString(token.getMin()));
            token_max.setText(ExPLoRAAService.convertTimeToString(token.getMax()));
            final LessonModel.StimulusTemplate stimulus = ctx.getLesson().model.stimuli.get(token.getToken().refEvent);
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
