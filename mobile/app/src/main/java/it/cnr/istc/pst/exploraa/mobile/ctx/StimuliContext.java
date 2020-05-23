package it.cnr.istc.pst.exploraa.mobile.ctx;

import androidx.annotation.NonNull;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import it.cnr.istc.pst.exploraa.api.Message.Stimulus;

public class StimuliContext {

    private static final StimuliContext instance = new StimuliContext();
    /**
     * The stimuli received so far.
     */
    private final List<Stimulus> stimuli = new ArrayList<>();
    private final Collection<StimuliListener> listeners = new ArrayList<>();

    public static StimuliContext getInstance() {
        return instance;
    }

    public void addStimulus(@NonNull final Stimulus stimulus) {
        stimuli.add(stimulus);
        for (StimuliListener listener : listeners)
            listener.stimulusAdded(stimulus);
    }

    public void removeStimulus(@NonNull final Stimulus stimulus) {
        stimuli.remove(stimulus);
        for (StimuliListener listener : listeners)
            listener.stimulusRemoved(stimulus);
    }

    public Collection<Stimulus> getStimuli() {
        return Collections.unmodifiableCollection(stimuli);
    }

    public void clear() {
        stimuli.clear();
        for (StimuliListener listener : listeners)
            listener.stimuliCleared();
    }

    public void addStimuliListener(StimuliListener l) {
        listeners.add(l);
    }

    public void removeStimuliListener(StimuliListener l) {
        listeners.remove(l);
    }

    public interface StimuliListener {

        void stimulusAdded(Stimulus stimulus);

        void stimulusRemoved(Stimulus stimulus);

        void stimuliCleared();
    }
}
