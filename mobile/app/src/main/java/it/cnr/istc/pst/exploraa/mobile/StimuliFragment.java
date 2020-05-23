package it.cnr.istc.pst.exploraa.mobile;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.recyclerview.widget.DividerItemDecoration;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;
import androidx.recyclerview.widget.SortedList;

import it.cnr.istc.pst.exploraa.api.Message;
import it.cnr.istc.pst.exploraa.mobile.ctx.StimuliContext;

public class StimuliFragment extends Fragment implements StimuliContext.StimuliListener {

    private final StimuliAdapter stimuli_adapter = new StimuliAdapter();
    private RecyclerView stimuli_recycler_view;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        stimuli_adapter.stimuli.addAll(StimuliContext.getInstance().getStimuli());
        StimuliContext.getInstance().addStimuliListener(this);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        StimuliContext.getInstance().removeStimuliListener(this);
    }

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
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

    @Override
    public void stimulusAdded(Message.Stimulus stimulus) {
        stimuli_adapter.stimuli.add(stimulus);
    }

    @Override
    public void stimulusRemoved(Message.Stimulus stimulus) {
        stimuli_adapter.stimuli.remove(stimulus);
    }

    @Override
    public void stimuliCleared() {
        stimuli_adapter.stimuli.clear();
    }

    private static class StimulusView extends RecyclerView.ViewHolder implements View.OnClickListener {

        private TextView title;
        private Message.Stimulus stimulus;

        private StimulusView(View view) {
            super(view);
            view.setOnClickListener(this);
            title = view.findViewById(R.id.stimulus_title);
        }

        private void setStimulus(Message.Stimulus stimulus) {
            this.stimulus = stimulus;
            if (stimulus instanceof Message.Stimulus.TextStimulus)
                title.setText(((Message.Stimulus.TextStimulus) stimulus).getContent());
            else if (stimulus instanceof Message.Stimulus.QuestionStimulus)
                title.setText(((Message.Stimulus.QuestionStimulus) stimulus).getQuestion());
            else if (stimulus instanceof Message.Stimulus.URLStimulus)
                title.setText(((Message.Stimulus.URLStimulus) stimulus).getContent());
        }

        @Override
        public void onClick(View v) {
        }
    }

    private class StimuliAdapter extends RecyclerView.Adapter<StimulusView> {

        private SortedList<Message.Stimulus> stimuli;

        public StimuliAdapter() {
            this.stimuli = new SortedList<>(Message.Stimulus.class, new SortedList.Callback<Message.Stimulus>() {
                @Override
                public int compare(Message.Stimulus o1, Message.Stimulus o2) {
                    return Long.compare(o1.getTime(), o2.getTime());
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
}
