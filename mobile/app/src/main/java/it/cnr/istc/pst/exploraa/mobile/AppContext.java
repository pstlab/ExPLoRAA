package it.cnr.istc.pst.exploraa.mobile;

public class AppContext {

    private static final AppContext instance = new AppContext();

    public static AppContext getInstance() {
        return instance;
    }

    private AppContext() {
    }
}
