package it.cnr.istc.exploraa;

import android.content.Intent;
import android.os.Bundle;
import android.support.annotation.NonNull;
import android.support.v7.app.AppCompatActivity;
import android.support.v7.widget.LinearLayoutManager;
import android.support.v7.widget.RecyclerView;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.CheckBox;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class TopicsActivity extends AppCompatActivity {

    private static final String TAG = "EnrollActivity";
    private RecyclerView topics_recycler_view;
    private Button choose_topics_button;
    private TopicsAdapter topics_adapter;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_topics);

        topics_recycler_view = findViewById(R.id.topics_recycler_view);
        choose_topics_button = findViewById(R.id.choose_topics_button);

        topics_adapter = new TopicsAdapter(this, getIntent().getCharSequenceArrayListExtra("topics"));

        // use this setting to improve performance if you know that changes
        // in content do not change the layout size of the RecyclerView
        topics_recycler_view.setHasFixedSize(true);
        topics_recycler_view.setLayoutManager(new LinearLayoutManager(this));
        topics_recycler_view.setAdapter(topics_adapter);
    }

    public void choose_topics(View v) {
        Intent data = new Intent();
        data.putCharSequenceArrayListExtra("topics", new ArrayList<CharSequence>(topics_adapter.selected_topics));
        setResult(RESULT_OK, data);
        finish();
    }

    private static class TopicsAdapter extends RecyclerView.Adapter<TopicView> {

        private final Set<CharSequence> selected_topics = new HashSet<>();
        private final List<CharSequence> topics;
        private TopicsActivity activity;

        private TopicsAdapter(TopicsActivity activity, Collection<CharSequence> topics) {
            this.activity = activity;
            this.topics = new ArrayList<>(topics);
        }

        @NonNull
        @Override
        public TopicView onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
            return new TopicView(activity, LayoutInflater.from(parent.getContext()).inflate(R.layout.topic_row, parent, false));
        }

        @Override
        public void onBindViewHolder(@NonNull TopicView holder, int position) {
            holder.topic.setText(topics.get(position));
        }

        @Override
        public int getItemCount() {
            return topics.size();
        }
    }

    private static class TopicView extends RecyclerView.ViewHolder implements View.OnClickListener {

        private TopicsActivity activity;
        private CheckBox topic;

        private TopicView(final TopicsActivity activity, View view) {
            super(view);
            this.activity = activity;
            topic = view.findViewById(R.id.topic_check_box);
            topic.setOnClickListener(this);
        }

        @Override
        public void onClick(View v) {
            final CharSequence c_topic = activity.topics_adapter.topics.get(getAdapterPosition());
            if (topic.isChecked()) {
                activity.topics_adapter.selected_topics.add(c_topic);
                activity.choose_topics_button.setEnabled(true);
            } else {
                activity.topics_adapter.selected_topics.remove(c_topic);
                if (activity.topics_adapter.selected_topics.isEmpty())
                    activity.choose_topics_button.setEnabled(false);
            }
        }
    }
}
