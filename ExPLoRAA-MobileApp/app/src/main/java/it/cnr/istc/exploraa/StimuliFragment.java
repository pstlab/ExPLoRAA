package it.cnr.istc.exploraa;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.os.Bundle;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.support.v4.app.Fragment;
import android.support.v7.widget.DividerItemDecoration;
import android.support.v7.widget.LinearLayoutManager;
import android.support.v7.widget.RecyclerView;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import java.util.ArrayList;
import java.util.List;

import it.cnr.istc.exploraa.api.Message;

public class StimuliFragment extends Fragment {

    private RecyclerView stimuli_recycler_view;
    private final StimuliAdapter stimuli_adapter = new StimuliAdapter();
    private BroadcastReceiver stimulus_added_receiver = new BroadcastReceiver() {
        @Override
        public void onReceive(Context context, Intent intent) {
            stimuli_adapter.notifyItemInserted(intent.getIntExtra("position", 0));
        }
    };
    private BroadcastReceiver stimulus_removed_receiver = new BroadcastReceiver() {
        @Override
        public void onReceive(Context context, Intent intent) {
            stimuli_adapter.notifyItemRemoved(intent.getIntExtra("position", 0));
        }
    };
    private BroadcastReceiver stimului_cleared_receiver = new BroadcastReceiver() {
        @Override
        public void onReceive(Context context, Intent intent) {
            stimuli_adapter.notifyDataSetChanged();
        }
    };

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        assert getActivity() != null;
        getActivity().registerReceiver(stimulus_added_receiver, new IntentFilter(ExPLoRAAService.ADDED_STIMULUS));
        getActivity().registerReceiver(stimulus_removed_receiver, new IntentFilter(ExPLoRAAService.REMOVED_STIMULUS));
        getActivity().registerReceiver(stimului_cleared_receiver, new IntentFilter(ExPLoRAAService.CLEARED_STIMULI));
    }

    @Override
    public void onDestroy() {
        super.onDestroy();

        assert getActivity() != null;
        getActivity().unregisterReceiver(stimulus_added_receiver);
        getActivity().unregisterReceiver(stimulus_removed_receiver);
        getActivity().unregisterReceiver(stimului_cleared_receiver);
    }

    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        // Inflate the layout for this fragment
        return inflater.inflate(R.layout.fragment_stimuli, container, false);
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        stimuli_recycler_view = view.findViewById(R.id.stimuli_recycler_view);

        // use this setting to improve performance if you know that changes
        // in content do not change the layout size of the RecyclerView
        stimuli_recycler_view.setHasFixedSize(true);
        stimuli_recycler_view.setLayoutManager(new LinearLayoutManager(getContext()));
        stimuli_recycler_view.setAdapter(stimuli_adapter);
        stimuli_recycler_view.addItemDecoration(new DividerItemDecoration(getContext(), DividerItemDecoration.VERTICAL));
    }

    void setStimuli(List<Message.Stimulus> stimuli) {
        stimuli_adapter.setStimuli(stimuli);
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
                    final Intent text_intent = new Intent(getContext(), TextStimulusActivity.class);
                    text_intent.putExtra("content", ((Message.Stimulus.TextStimulus) stimulus).content);
                    startActivity(text_intent);
                    break;
                case Question:
                    final Intent question_intent = new Intent(getContext(), TextStimulusActivity.class);
                    question_intent.putExtra("question", ((Message.Stimulus.QuestionStimulus) stimulus).question);
                    ArrayList<CharSequence> answers = new ArrayList<>(((Message.Stimulus.QuestionStimulus) stimulus).answers.size());
                    answers.addAll(((Message.Stimulus.QuestionStimulus) stimulus).answers);
                    question_intent.putExtra("answers", answers);
                    if (((Message.Stimulus.QuestionStimulus) stimulus).answer != null) {
                        question_intent.putExtra("answer", ((Message.Stimulus.QuestionStimulus) stimulus).answer);
                    }
                    startActivity(question_intent);
                    break;
                case URL:
                    final Intent url_intent = new Intent(getContext(), TextStimulusActivity.class);
                    url_intent.putExtra("content", ((Message.Stimulus.URLStimulus) stimulus).content);
                    url_intent.putExtra("url", ((Message.Stimulus.URLStimulus) stimulus).url);
                    startActivity(url_intent);
                    break;
            }
        }
    }
}
