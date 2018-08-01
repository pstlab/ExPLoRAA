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
import android.widget.CheckBox;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class TopicsActivity extends AppCompatActivity {

    private static final String TAG = "EnrollActivity";
    private RecyclerView topics_recycler_view;
    private TopicsAdapter topics_adapter;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_topics);

        topics_recycler_view = findViewById(R.id.topics_recycler_view);
        topics_adapter = new TopicsAdapter(getIntent().getCharSequenceArrayListExtra("topics"));

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

        private TopicsAdapter(Collection<CharSequence> topics) {
            this.topics = new ArrayList<>(topics);
        }

        @NonNull
        @Override
        public TopicView onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
            return new TopicView(this, LayoutInflater.from(parent.getContext()).inflate(R.layout.topic_row, parent, false));
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

    private static class TopicView extends RecyclerView.ViewHolder {

        private CheckBox topic;

        private TopicView(final TopicsAdapter adapter, View view) {
            super(view);
            topic = view.findViewById(R.id.topic_check_box);
            topic.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View v) {
                    final CharSequence c_topic = adapter.topics.get(getAdapterPosition());
                    if (topic.isChecked()) adapter.selected_topics.add(c_topic);
                    else adapter.selected_topics.remove(c_topic);
                }
            });
        }
    }
}
