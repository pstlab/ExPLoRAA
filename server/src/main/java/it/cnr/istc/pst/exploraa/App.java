package it.cnr.istc.pst.exploraa;

import java.util.concurrent.Executor;
import java.util.concurrent.Executors;

import io.javalin.Javalin;

/**
 * Hello world!
 *
 */
public class App {

    static final Executor EXECUTOR = Executors.newSingleThreadExecutor();

    public static void main(String[] args) {
        final Javalin app = Javalin.create(config -> {
            config.addStaticFiles("/public");
        });
        app.start();
    }
}
