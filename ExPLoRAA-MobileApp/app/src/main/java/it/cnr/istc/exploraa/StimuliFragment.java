package it.cnr.istc.exploraa;

import android.content.Intent;
import android.os.Bundle;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.support.v4.app.Fragment;
import android.support.v7.util.SortedList;
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

public class StimuliFragment extends Fragment implements ExPLoRAAService.StimuliListener {

    private RecyclerView stimuli_recycler_view;
    private final StimuliAdapter stimuli_adapter = new StimuliAdapter();

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        ExPLoRAAContext.getInstance().getService().addStimuliListener(this);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        if (ExPLoRAAContext.getInstance().isServiceRunning())
            ExPLoRAAContext.getInstance().getService().removeStimuliListener(this);
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
        stimuli_adapter.stimuli.addAll(stimuli);
    }

    @Override
    public void stimulusAdded(int pos, Message.Stimulus stimulus) {
        stimuli_adapter.stimuli.add(stimulus);
    }

    @Override
    public void stimulusRemoved(int pos, Message.Stimulus stimulus) {
        stimuli_adapter.stimuli.remove(stimulus);
    }

    @Override
    public void stimuliCleared() {
        stimuli_adapter.stimuli.clear();
    }

    private class StimuliAdapter extends RecyclerView.Adapter<StimulusView> {

        private SortedList<Message.Stimulus> stimuli;

        public StimuliAdapter() {
            this.stimuli = new SortedList<>(Message.Stimulus.class, new SortedList.Callback<Message.Stimulus>() {
                @Override
                public int compare(Message.Stimulus o1, Message.Stimulus o2) {
                    return Long.compare(o1.time, o2.time);
                }

                @Override
                public void onChanged(int position, int count) {
                    notifyItemRangeChanged(position, count);
                }

                @Override
                public boolean areContentsTheSame(Message.Stimulus oldItem, Message.Stimulus newItem) {
                    return false;
                }

                @Override
                public boolean areItemsTheSame(Message.Stimulus item1, Message.Stimulus item2) {
                    return item1 == item2;
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
                    final Intent question_intent = new Intent(getContext(), QuestionStimulusActivity.class);
                    question_intent.putExtra("lesson_id", stimulus.lesson_id);
                    question_intent.putExtra("question_id", stimulus.id);
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
                    final Intent url_intent = new Intent(getContext(), URLStimulusActivity.class);
                    url_intent.putExtra("content", ((Message.Stimulus.URLStimulus) stimulus).content);
                    url_intent.putExtra("url", ((Message.Stimulus.URLStimulus) stimulus).url);
                    startActivity(url_intent);
                    break;
            }
        }
    }
}
