/*
 * Copyright (C) 2018 Riccardo De Benedictis
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package it.cnr.istc.pst.exploraa.webapp;

import it.cnr.istc.pst.exploraa.api.Lesson;
import it.cnr.istc.pst.exploraa.api.LessonModel;
import it.cnr.istc.pst.time.TemporalListener;
import it.cnr.istc.pst.time.TemporalNetwork;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author Riccardo De Benedictis
 */
public class LessonManager implements TemporalListener {

    private static final Logger LOG = Logger.getLogger(LessonManager.class.getName());
    public static final String THIS = "this";
    private final Lesson lesson;
    public final TemporalNetwork network = new TemporalNetwork(16);
    private final Map<String, LessonModel.StimulusTemplate> event_templates = new HashMap<>();
    /**
     * For each time point, the corresponding token.
     */
    private final List<SolverToken> tokens = new ArrayList<>();
    /**
     * A collection of triggerable tokens. These tokens are expanded whenever
     * their triggering condition becomes satisfied.
     */
    private final Collection<SolverToken> triggerable_tokens = new ArrayList<>();
    /**
     * The current context, if any..
     */
    private TriggeredContext triggered_context = null;
    private final Map<SolverToken, TriggeredContext> triggered_contexts = new IdentityHashMap<>();
    private final Deque<SolverToken> prop_q = new ArrayDeque<>();
    private final List<Long> lesson_timeline_pulses = new ArrayList<>();
    private final List<Collection<SolverToken>> lesson_timeline_values = new ArrayList<>();
    private long t_now = 0;
    private int idx = 0;
    private final Collection<LessonManagerListener> listeners = new ArrayList<>();

    public LessonManager(Lesson lesson) {
        this.lesson = lesson;
        network.addTemporalListener(this);
    }

    public Lesson getLesson() {
        return lesson;
    }

    public void makeTriggerable(SolverToken tk) {
        triggerable_tokens.add(tk);
    }

    public Collection<SolverToken> getTriggerableTokens() {
        return Collections.unmodifiableCollection(triggerable_tokens);
    }

    public boolean isTriggerable(SolverToken tk) {
        return triggerable_tokens.contains(tk);
    }

    public void makeUntriggerable(SolverToken tk) {
        triggerable_tokens.remove(tk);
    }

    public void solve() {
        for (LessonModel.StimulusTemplate event_template : lesson.model.stimuli.values()) {
            if (event_templates.containsKey(event_template.name)) {
                LOG.log(Level.WARNING, "Renaming event {0}", event_template.name);
            }
            event_templates.put(event_template.name, event_template);
        }

        Map<String, SolverToken> c_tks = new HashMap<>();
        // we create the tokens..
        for (String id : lesson.model.ids) {
            SolverToken tk = new SolverToken(null, network.newTimePoint(), event_templates.get(id), null);
            tokens.add(tk);
            listeners.forEach(l -> l.newToken(tk));
            c_tks.put(id, tk);
            prop_q.push(tk);
        }

        // we enforce the temporal relations..
        for (LessonModel.Relation rel : lesson.model.relations) {
            double lb = rel.lb != null ? TimeUnit.MILLISECONDS.convert(rel.lb, rel.unit) : Double.NEGATIVE_INFINITY;
            double ub = rel.ub != null ? TimeUnit.MILLISECONDS.convert(rel.ub, rel.unit) : Double.POSITIVE_INFINITY;
            if (rel.from.equals(THIS)) {
                network.addConstraint(0, c_tks.get(rel.to).tp, lb, ub);
            } else {
                network.addConstraint(c_tks.get(rel.from).tp, c_tks.get(rel.to).tp, lb, ub);
            }
        }

        // we build the lesson..
        build();

        // we extract the lesson timeline..
        extract_timeline();
    }

    private void expand_token(final SolverToken tk) {
        Map<String, SolverToken> c_tks = new HashMap<>();
        c_tks.put(THIS, tk);
        if (tk.template.ids != null) {
            // we create the (sub) tokens..
            for (String id : tk.template.ids) {
                SolverToken c_tk = new SolverToken(tk, network.newTimePoint(), event_templates.get(id), null);
                tokens.add(c_tk);
                listeners.forEach(l -> l.newToken(c_tk));
                c_tks.put(id, c_tk);
                prop_q.push(c_tk);
            }
        }

        if (tk.template.relations != null) {
            // we enforce the temporal relations..
            for (LessonModel.Relation rel : tk.template.relations) {
                double lb = rel.lb != null ? TimeUnit.MILLISECONDS.convert(rel.lb, rel.unit) : Double.NEGATIVE_INFINITY;
                double ub = rel.ub != null ? TimeUnit.MILLISECONDS.convert(rel.ub, rel.unit) : Double.POSITIVE_INFINITY;
                if (rel.from.equals(THIS)) {
                    network.addConstraint(tk.tp, c_tks.get(rel.to).tp, lb, ub);
                } else {
                    network.addConstraint(c_tks.get(rel.from).tp, c_tks.get(rel.to).tp, lb, ub);
                }
            }
        }

        if (triggered_context != null) {
            triggered_context.tokens.add(tk);
        }
    }

    private void build() {
        while (!prop_q.isEmpty()) {
            SolverToken tk = prop_q.pop();
            if (tk.template.trigger_condition == null) {
                expand_token(tk);
            }
        }
        // we propagate the temporal network..
        network.propagate();
    }

    private void extract_timeline() {
        lesson_timeline_pulses.clear();
        lesson_timeline_values.clear();
        Set<Long> c_pulses = new HashSet<>();
        Map<Long, Collection<SolverToken>> at = new HashMap<>();
        tokens.stream().filter(tk -> tk.enabled).forEach(tk -> {
            long pulse = (long) network.value(tk.tp);
            c_pulses.add(pulse);
            Collection<SolverToken> tks = at.get(pulse);
            if (tks == null) {
                tks = new ArrayList<>();
                at.put(pulse, tks);
            }
            tks.add(tk);
        });
        Long[] c_arr_pulses = c_pulses.toArray(new Long[c_pulses.size()]);
        Arrays.sort(c_arr_pulses);
        for (Long pulse : c_arr_pulses) {
            lesson_timeline_pulses.add(pulse);
            lesson_timeline_values.add(at.get(pulse));
        }
    }

    public void setTime(final int var, final double value) {
        if (value < network.lb(var)) {
            network.setValue(var, network.lb(var));
        } else if (value > network.ub(var)) {
            network.setValue(var, network.ub(var));
        } else {
            network.setValue(var, value);
        }

        // we extract the lesson timeline..
        extract_timeline();
    }

    /**
     * Executes one tick. In other words moves the execution of the lesson
     * forward of one second.
     */
    public void tick() {
        goTo(t_now + 1000);
    }

    /**
     * Executes the lesson, either forward or backward, till the given relative
     * (to the lesson) time.
     *
     * @param t the relative current time.
     */
    public void goTo(final long t) {
        lesson.time = t;
        if (t > t_now && idx < lesson_timeline_pulses.size()) {
            // we are moving forward..
            long next_pulse = lesson_timeline_pulses.get(idx);
            while (next_pulse <= t) {
                for (SolverToken tk : lesson_timeline_values.get(idx)) {
                    // this token can be executed..
                    listeners.forEach(l -> l.executeToken(tk));
                }
                idx++;
                if (idx < lesson_timeline_pulses.size()) {
                    next_pulse = lesson_timeline_pulses.get(idx);
                } else {
                    // we have no more tokens to execute..
                    break;
                }
            }
        }
        if (t < t_now && idx > 0) {
            // we are moving backward..
            long last_pulse = lesson_timeline_pulses.get(idx - 1);
            // these are the potential consequences of the triggered tokens..
            Collection<SolverToken> tr_consequences = new ArrayList<>();
            while (last_pulse > t) {
                for (SolverToken tk : lesson_timeline_values.get(idx - 1)) {
                    TriggeredContext ctx = triggered_contexts.remove(tk);
                    if (ctx != null) {
                        // token 'tk' is a triggered token, we remove all the consequences of the triggered context..
                        tr_consequences.addAll(ctx.tokens);
                    }
                    // this token has been executed.. so we hide it..
                    listeners.forEach(l -> l.hideToken(tk));
                }
                idx--;
                if (idx > 0) {
                    last_pulse = lesson_timeline_pulses.get(idx - 1);
                } else {
                    // we have no more tokens to hide..
                    break;
                }
            }

            if (!tr_consequences.isEmpty()) {
                // we remove these tokens and re-extract the lesson timeline..
                for (SolverToken tk : tr_consequences) {
                    tk.enabled = false;
                    listeners.forEach(l -> l.removeToken(tk));
                }

                // we re-extract the lesson timeline..
                extract_timeline();
            }
        }
        t_now = t;
        listeners.forEach(l -> l.newTime(t_now));
    }

    public void answerQuestion(long user_id, final int question_id, final int answer) {
        SolverToken q_tk = tokens.get(question_id - 2);
        LessonModel.StimulusTemplate.QuestionStimulusTemplate.Answer answr = ((LessonModel.StimulusTemplate.QuestionStimulusTemplate) q_tk.template).answers.get(answer);

        TriggeredContext ctx = new TriggeredContext(q_tk);
        this.triggered_context = ctx;

        // this token represents the effects of the answer on the lesson..
        SolverToken c_tk = new SolverToken(null, network.newTimePoint(), event_templates.get(answr.event), question_id);
        tokens.add(c_tk);
        listeners.forEach(l -> l.newToken(c_tk));
        prop_q.push(c_tk);

        network.addConstraint(0, c_tk.tp, t_now + 1000, t_now + 1000);
        build();

        triggered_contexts.put(c_tk, ctx);
        this.triggered_context = null;

        // we extract the lesson timeline..
        extract_timeline();
    }

    public void trigger(long user_id, SolverToken tk) {
        triggerable_tokens.remove(tk);

        TriggeredContext ctx = new TriggeredContext(tk);
        this.triggered_context = ctx;

        // this token represents the effects of the triggered token on the lesson..
        SolverToken c_tk = new SolverToken(null, network.newTimePoint(), tk.template, null);
        tokens.add(c_tk);
        listeners.forEach(l -> l.newToken(c_tk));
        prop_q.push(c_tk);

        network.addConstraint(0, c_tk.tp, t_now + 1000, t_now + 1000);
        build();

        triggered_contexts.put(c_tk, ctx);
        this.triggered_context = null;

        // we extract the lesson timeline..
        extract_timeline();
    }

    @Override
    public void newValue(int tp, double val) {
        if (tp != 0 && tp != 1) {
            listeners.forEach(l -> l.movedToken(tokens.get(tp - 2)));
        }
    }

    @Override
    public void boundChange(int tp, double min, double max) {
        if (tp != 0 && tp != 1) {
            listeners.forEach(l -> l.movedToken(tokens.get(tp - 2)));
        }
    }

    @Override
    public void distanceChange(int tp_from, int tp_to, double min, double max) {
    }

    public void addSolverListener(LessonManagerListener listener) {
        listeners.add(listener);
    }

    public void removeSolverListener(LessonManagerListener listener) {
        listeners.remove(listener);
    }

    private class TriggeredContext {

        private final SolverToken source_token;
        final Collection<SolverToken> tokens = new ArrayList<>();

        private TriggeredContext(SolverToken question) {
            this.source_token = question;
        }

        public SolverToken getSourceToken() {
            return source_token;
        }

        public Collection<SolverToken> getTokens() {
            return Collections.unmodifiableCollection(tokens);
        }
    }

    public static class SolverToken {

        /**
         * This is the cause for having this token. It is the token whose
         * expansion introduced this token.
         */
        public final SolverToken cause;
        /**
         * This is the temporal variable associated to the token.
         */
        public final int tp;
        /**
         * This is the template of the token.
         */
        public final LessonModel.StimulusTemplate template;
        /**
         * An {@code Integer} representing the id of the question, if any, or
         * {@code null} if the token does not represent an answer to a question.
         */
        public final Integer question;
        boolean enabled = true;

        SolverToken(final SolverToken cause, final int tp, final LessonModel.StimulusTemplate template, Integer question) {
            this.cause = cause;
            this.tp = tp;
            this.template = template;
            this.question = question;
        }

        public boolean isEnabled() {
            return enabled;
        }
    }
}
