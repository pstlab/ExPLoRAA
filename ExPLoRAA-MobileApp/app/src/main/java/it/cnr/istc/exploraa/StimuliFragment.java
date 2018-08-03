package it.cnr.istc.exploraa;

import android.content.Intent;
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

import it.cnr.istc.exploraa.api.Message;

public class StimuliFragment extends Fragment {

    private RecyclerView stimuli_recycler_view;
    private StimuliAdapter stimuli_adapter;

    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        // Inflate the layout for this fragment
        return inflater.inflate(R.layout.fragment_stimuli, container, false);
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        stimuli_recycler_view = view.findViewById(R.id.stimuli_recycler_view);
        stimuli_adapter = new StimuliAdapter(this);

        // use this setting to improve performance if you know that changes
        // in content do not change the layout size of the RecyclerView
        stimuli_recycler_view.setHasFixedSize(true);
        stimuli_recycler_view.setLayoutManager(new LinearLayoutManager(getContext()));
        stimuli_recycler_view.setAdapter(stimuli_adapter);
        stimuli_recycler_view.addItemDecoration(new DividerItemDecoration(getContext(), DividerItemDecoration.HORIZONTAL));
    }

    @Override
    public void onResume() {
        super.onResume();
        ExPLoRAAContext.getInstance().addStimuliListener(stimuli_adapter);
    }

    @Override
    public void onPause() {
        super.onPause();
        ExPLoRAAContext.getInstance().removeStimuliListener(stimuli_adapter);
    }

    private static class StimuliAdapter extends RecyclerView.Adapter<StimulusView> implements ExPLoRAAContext.StimuliListener {

        private StimuliFragment frgmnt;

        public StimuliAdapter(StimuliFragment frgmnt) {
            this.frgmnt = frgmnt;
        }

        @NonNull
        @Override
        public StimulusView onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
            return new StimulusView(frgmnt, LayoutInflater.from(parent.getContext()).inflate(R.layout.stimulus_row, parent, false));
        }

        @Override
        public void onBindViewHolder(@NonNull StimulusView holder, int position) {
            holder.setStimulus(ExPLoRAAContext.getInstance().getStimuli().get(position));
        }

        @Override
        public int getItemCount() {
            return ExPLoRAAContext.getInstance().getStimuli().size();
        }

        @Override
        public void stimulusAdded(int pos, Message.Stimulus stimulus) {
            notifyItemInserted(pos);
        }

        @Override
        public void stimulusRemoved(int pos, Message.Stimulus stimulus) {
            notifyItemRemoved(pos);
        }

        @Override
        public void stimuliCleared() {
            notifyDataSetChanged();
        }
    }

    private static class StimulusView extends RecyclerView.ViewHolder implements View.OnClickListener {

        private StimuliFragment frgmnt;
        private TextView title;
        private Message.Stimulus stimulus;

        private StimulusView(StimuliFragment frgmnt, View view) {
            super(view);
            this.frgmnt = frgmnt;
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
                    final Intent text_intent = new Intent(frgmnt.getContext(), TextStimulusActivity.class);
                    text_intent.putExtra("content", ((Message.Stimulus.TextStimulus) stimulus).content);
                    frgmnt.startActivity(text_intent);
                    break;
                case Question:
                    final Intent question_intent = new Intent(frgmnt.getContext(), TextStimulusActivity.class);
                    question_intent.putExtra("question", ((Message.Stimulus.QuestionStimulus) stimulus).question);
                    ArrayList<CharSequence> answers = new ArrayList<>(((Message.Stimulus.QuestionStimulus) stimulus).answers.size());
                    for (String answer : ((Message.Stimulus.QuestionStimulus) stimulus).answers)
                        answers.add(answer);
                    question_intent.putExtra("answers", answers);
                    if (((Message.Stimulus.QuestionStimulus) stimulus).answer != null) {
                        question_intent.putExtra("answer", ((Message.Stimulus.QuestionStimulus) stimulus).answer);
                    }
                    frgmnt.startActivity(question_intent);
                    break;
                case URL:
                    final Intent url_intent = new Intent(frgmnt.getContext(), TextStimulusActivity.class);
                    url_intent.putExtra("content", ((Message.Stimulus.URLStimulus) stimulus).content);
                    url_intent.putExtra("url", ((Message.Stimulus.URLStimulus) stimulus).url);
                    frgmnt.startActivity(url_intent);
                    break;
            }
        }
    }
}
