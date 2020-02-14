package it.cnr.istc.pst.exploraa.server.solver;

/**
 * TemporalListener
 */
public interface TemporalListener {

    public void newValue(int tp, double val);

    public void boundChange(int tp, double min, double max);

    public void distanceChange(int tp_from, int tp_to, double min, double max);
}