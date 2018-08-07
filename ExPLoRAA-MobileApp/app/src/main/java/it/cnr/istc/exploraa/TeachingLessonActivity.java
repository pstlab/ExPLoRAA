package it.cnr.istc.exploraa;

import android.content.BroadcastReceiver;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
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
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

import it.cnr.istc.exploraa.api.Lesson;
import it.cnr.istc.exploraa.api.LessonModel;

public class TeachingLessonActivity extends AppCompatActivity {

    private static final String TAG = "TeachingLessonActivity";
    private long lesson_id;
    private TeachingLessonContext ctx;
    private Menu options_menu;
    private ImageView teaching_lesson_status_image_view;
    private TextView teaching_lesson_name;
    private TextView teaching_lesson_time;
    private TokensAdapter teaching_lesson_tokens_adapter;
    private ExPLoRAAService service;
    private ServiceConnection service_connection = new ServiceConnection() {
        @Override
        public void onServiceConnected(ComponentName name, IBinder binder) {
            service = ((ExPLoRAAService.ExPLoRAABinder) binder).getService();

            ctx = service.getTeachingLesson(lesson_id);
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
        }

        @Override
        public void onServiceDisconnected(ComponentName name) {
            service = null;
        }
    };
    private BroadcastReceiver time_receiver = new BroadcastReceiver() {
        @Override
        public void onReceive(Context context, Intent intent) {
            teaching_lesson_time.setText(ExPLoRAAService.convertTimeToString(intent.getLongExtra("time", 0)));
        }
    };
    private BroadcastReceiver state_receiver = new BroadcastReceiver() {
        @Override
        public void onReceive(Context context, Intent intent) {
            switch (Lesson.LessonState.valueOf(intent.getStringExtra("state"))) {
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
    };
    private BroadcastReceiver token_added_receiver = new BroadcastReceiver() {
        @Override
        public void onReceive(Context context, Intent intent) {
            teaching_lesson_tokens_adapter.notifyItemInserted(intent.getIntExtra("position", 0));
        }
    };
    private BroadcastReceiver token_updated_receiver = new BroadcastReceiver() {
        @Override
        public void onReceive(Context context, Intent intent) {
            teaching_lesson_tokens_adapter.notifyItemChanged(intent.getIntExtra("position", 0));
        }
    };
    private BroadcastReceiver token_removed_receiver = new BroadcastReceiver() {
        @Override
        public void onReceive(Context context, Intent intent) {
            teaching_lesson_tokens_adapter.notifyItemRemoved(intent.getIntExtra("position", 0));
        }
    };

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_teaching_lesson);

        teaching_lesson_status_image_view = findViewById(R.id.activity_teaching_lesson_status_image_view);
        teaching_lesson_name = findViewById(R.id.activity_teaching_lesson_name);
        teaching_lesson_time = findViewById(R.id.activity_teaching_lesson_time);
        RecyclerView teaching_lesson_tokens_recycler_view = findViewById(R.id.activity_teaching_lesson_tokens_recycler_view);

        lesson_id = getIntent().getLongExtra("lesson_id", -1);

        teaching_lesson_tokens_adapter = new TokensAdapter();

        // use this setting to improve performance if you know that changes
        // in content do not change the layout size of the RecyclerView
        teaching_lesson_tokens_recycler_view.setHasFixedSize(true);
        teaching_lesson_tokens_recycler_view.setLayoutManager(new LinearLayoutManager(this));
        teaching_lesson_tokens_recycler_view.setAdapter(teaching_lesson_tokens_adapter);
        teaching_lesson_tokens_recycler_view.addItemDecoration(new DividerItemDecoration(this, DividerItemDecoration.VERTICAL));

        registerReceiver(time_receiver, new IntentFilter(TeachingLessonContext.TEACHING_LESSON_TIME_CHANGED + lesson_id));
        registerReceiver(state_receiver, new IntentFilter(TeachingLessonContext.TEACHING_LESSON_STATE_CHANGED + lesson_id));
        registerReceiver(token_added_receiver, new IntentFilter(TeachingLessonContext.ADDED_LESSON_TOKEN + lesson_id));
        registerReceiver(token_updated_receiver, new IntentFilter(TeachingLessonContext.UPDATED_LESSON_TOKEN + lesson_id));
        registerReceiver(token_removed_receiver, new IntentFilter(TeachingLessonContext.REMOVED_LESSON_TOKEN + lesson_id));

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
        unregisterReceiver(time_receiver);
        unregisterReceiver(state_receiver);
        unregisterReceiver(token_added_receiver);
        unregisterReceiver(token_updated_receiver);
        unregisterReceiver(token_removed_receiver);
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
                service.play(ctx.getLesson());
                return true;
            case R.id.teaching_lesson_pause_menu_item:
                service.pause(ctx.getLesson());
                return true;
            case R.id.teaching_lesson_stop_menu_item:
                service.stop(ctx.getLesson());
                return true;
            default:
                return super.onOptionsItemSelected(item);
        }
    }

    private class TokensAdapter extends RecyclerView.Adapter<TokenView> {

        @NonNull
        @Override
        public TokenView onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
            return new TokenView(LayoutInflater.from(parent.getContext()).inflate(R.layout.token_row, parent, false));
        }

        @Override
        public void onBindViewHolder(@NonNull TokenView holder, int position) {
            holder.setToken(ctx.getTokens().get(position));
        }

        @Override
        public int getItemCount() {
            return ctx.getTokens().size();
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
