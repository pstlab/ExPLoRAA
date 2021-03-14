package it.cnr.istc.pst.exploraa;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.Map;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import it.cnr.istc.pst.exploraa.api.Lesson.LessonState;
import it.cnr.istc.pst.exploraa.api.Message.Stimulus;
import it.cnr.istc.pst.exploraa.api.Message.Token;
import it.cnr.istc.pst.exploraa.db.LessonEntity;
import it.cnr.istc.pst.oratio.Atom;
import it.cnr.istc.pst.oratio.Item;
import it.cnr.istc.pst.oratio.Predicate;
import it.cnr.istc.pst.oratio.Solver;
import it.cnr.istc.pst.oratio.Type;

public class LessonManager {

    static final Logger LOG = LoggerFactory.getLogger(LessonManager.class);
    private final LessonEntity lesson;
    private final Solver solver = new Solver();
    private final Set<String> topics = new HashSet<>();
    private final Collection<Stimulus> stimuli = new ArrayList<>();
    private final Collection<Token> tokens = new ArrayList<>();
    private LessonState state = LessonState.Stopped;
    private long time = 0;
    private final Collection<LessonManagerListener> listeners = new ArrayList<>();

    public LessonManager(final LessonEntity lesson) {
        this.lesson = lesson;
    }

    public void solve() {
        solver.read(new String()); // we load the planning problem..
        solver.solve(); // we solve the planning problem..

        // we collect the atoms from the solution..
        final Map<Item, Collection<Atom>> atoms = new IdentityHashMap<>();
        for (final Type t : solver.getTypes().values()) {
            if (t.getName().equals("Lesson")) {
                t.getInstances().forEach(i -> atoms.put(i, new ArrayList<>()));
                for (final Predicate p : t.getPredicates().values())
                    p.getInstances().stream().map(atm -> (Atom) atm)
                            .filter(atm -> (atm.getState() == Atom.AtomState.Active)).forEach(atm -> {
                                final Item tau = atm.getTau();
                                if (tau instanceof Item.EnumItem)
                                    for (final Item val : ((Item.EnumItem) tau).getVals())
                                        atoms.get(val).add(atm);
                                else
                                    atoms.get(tau).add(atm);
                            });
            }
        }

        // we extract the tokens from the solution..
        for (final Map.Entry<Item, Collection<Atom>> i_atoms : atoms.entrySet())
            for (final Atom atom : i_atoms.getValue())
                try {
                    Token token = new Token(lesson.getId(), ((Item.ArithItem) atom.get("id")).getValue().intValue(),
                            ((Item.ArithItem) atom.get("start")).getValue().longValue());
                    tokens.add(token);
                    for (final LessonManagerListener listener : listeners)
                        listener.newToken(token);
                } catch (NoSuchFieldException e) {
                    LOG.error("Cannot find field", e);
                }
    }

    public void play() {
        state = LessonState.Running;
        for (final LessonManagerListener listener : listeners)
            listener.stateChanged(state);
    }

    public void pause() {
        state = LessonState.Paused;
        for (final LessonManagerListener listener : listeners)
            listener.stateChanged(state);
    }

    public void stop() {
        state = LessonState.Stopped;
        for (final LessonManagerListener listener : listeners)
            listener.stateChanged(state);

        time = 0;
        for (final LessonManagerListener listener : listeners)
            listener.timeChanged(time);
    }

    /**
     * @return the topics
     */
    public Set<String> getTopics() {
        return Collections.unmodifiableSet(topics);
    }

    /**
     * @return the stimuli
     */
    public Collection<Stimulus> getStimuli() {
        if (stimuli.isEmpty())
            return null;
        return Collections.unmodifiableCollection(stimuli);
    }

    /**
     * @return the tokens
     */
    public Collection<Token> getTokens() {
        if (tokens.isEmpty())
            return null;
        return Collections.unmodifiableCollection(tokens);
    }

    /**
     * @return the state
     */
    public LessonState getState() {
        return state;
    }

    /**
     * @return the time
     */
    public long getTime() {
        return time;
    }

    public void addLessonManagerListener(final LessonManagerListener listener) {
        listeners.add(listener);
    }

    public void removeLessonManagerListener(final LessonManagerListener listener) {
        listeners.remove(listener);
    }

    public interface LessonManagerListener {

        void newToken(final Token token);

        void stateChanged(final LessonState state);

        void timeChanged(final long time);
    }
}
