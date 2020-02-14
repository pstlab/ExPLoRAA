package it.cnr.istc.pst.exploraa.server.solver;

import it.cnr.istc.pst.exploraa.api.Lesson;
import it.cnr.istc.pst.exploraa.api.LessonModel;

/**
 * LessonManager
 */
public class LessonManager implements TemporalListener {

    public final Lesson lesson;
    public final TemporalNetwork network = new TemporalNetwork(16);

    public LessonManager(Lesson lesson) {
        this.lesson = lesson;
        network.addTemporalListener(this);
    }

    @Override
    public void newValue(int tp, double val) {
    }

    @Override
    public void boundChange(int tp, double min, double max) {
    }

    @Override
    public void distanceChange(int tp_from, int tp_to, double min, double max) {
    }

    public static class SolverToken {

        /**
         * This is the cause for having this token. It is the token whose expansion
         * introduced this token.
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

        SolverToken(final SolverToken cause, final int tp, final LessonModel.StimulusTemplate template) {
            this.cause = cause;
            this.tp = tp;
            this.template = template;
        }
    }
}