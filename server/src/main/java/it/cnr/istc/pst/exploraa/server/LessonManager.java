package it.cnr.istc.pst.exploraa.server;

import java.io.StringReader;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.Map;
import java.util.Set;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.stream.JsonReader;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import it.cnr.istc.oratio.Solver;
import it.cnr.istc.oratio.riddle.Atom;
import it.cnr.istc.oratio.riddle.Core;
import it.cnr.istc.oratio.riddle.CoreDeserializer;
import it.cnr.istc.oratio.riddle.Item;
import it.cnr.istc.oratio.riddle.Predicate;
import it.cnr.istc.oratio.riddle.Type;
import it.cnr.istc.pst.exploraa.api.Lesson.LessonState;
import it.cnr.istc.pst.exploraa.api.Message.Stimulus;
import it.cnr.istc.pst.exploraa.api.Message.Token;

/**
 * LessonManager
 */
public class LessonManager {

    static final Logger LOG = LoggerFactory.getLogger(LessonManager.class);
    private final CoreDeserializer deserializer = new CoreDeserializer();
    private final Gson gson = new GsonBuilder().registerTypeAdapter(Core.class, deserializer).create();
    private final long lesson_id;
    private final String model;
    private final Solver solver = new Solver();
    private Core core;
    private final Set<String> topics = new HashSet<>();
    private final Collection<Stimulus> stimuli = new ArrayList<>();
    private final Collection<Token> tokens = new ArrayList<>();
    private LessonState state = LessonState.Stopped;
    private long time = 0;
    private final Collection<LessonManagerListener> listeners = new ArrayList<>();

    public LessonManager(final long lesson_id, final String model) {
        this.lesson_id = lesson_id;
        this.model = model;
    }

    public void solve() {
        solver.read(model); // we load the planning problem..
        solver.solve(); // we solve the planning problem..
        final String state = solver.getState(); // we get the current state of the solver (i.e., the planning problem
                                                // solution)..

        core = new Core();
        core.read(model);
        deserializer.setCore(core);
        gson.fromJson(new JsonReader(new StringReader(state)), Core.class);

        // we collect the atoms from the solution..
        final Map<Item, Collection<Atom>> atoms = new IdentityHashMap<>();
        for (final Type t : core.getTypes().values()) {
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
            for (final Atom atom : i_atoms.getValue()) {
                final Token token = new Token(lesson_id, ((Item.ArithItem) atom.getExpr("id")).getValue().intValue(),
                        ((Item.ArithItem) atom.getExpr("start")).getValue().longValue());
                tokens.add(token);
                for (final LessonManagerListener listener : listeners)
                    listener.newToken(token);
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