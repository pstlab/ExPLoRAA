package it.cnr.istc.pst.exploraa.server;

import java.io.StringReader;

import javax.persistence.EntityManager;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.stream.JsonReader;

import it.cnr.istc.oratio.Solver;
import it.cnr.istc.oratio.riddle.Core;
import it.cnr.istc.oratio.riddle.CoreDeserializer;
import it.cnr.istc.pst.exploraa.api.Lesson;
import it.cnr.istc.pst.exploraa.server.db.LessonModelEntity;

/**
 * LessonManager
 */
public class LessonManager {

    private final CoreDeserializer deserializer = new CoreDeserializer();
    private final Gson gson = new GsonBuilder().registerTypeAdapter(Core.class, deserializer).create();
    private final Lesson lesson;
    private final String model;
    private final Solver solver = new Solver();
    private Core core;

    public LessonManager(Lesson lesson) {
        this.lesson = lesson;

        EntityManager em = App.EMF.createEntityManager();
        LessonModelEntity model_entity = em.find(LessonModelEntity.class, lesson.getModelId());
        this.model = model_entity.getModel();
    }

    public void solve() {
        solver.read(model);
        solver.solve();
        final String state = solver.getState();

        core = new Core();
        core.read(model);
        deserializer.setCore(core);
        gson.fromJson(new JsonReader(new StringReader(state)), Core.class);
    }
}