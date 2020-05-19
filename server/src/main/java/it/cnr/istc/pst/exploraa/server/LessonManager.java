package it.cnr.istc.pst.exploraa.server;

import java.io.StringReader;
import java.util.ArrayList;
import java.util.Collection;
import java.util.IdentityHashMap;
import java.util.Map;

import javax.persistence.EntityManager;

import com.fasterxml.jackson.core.JsonProcessingException;
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
import it.cnr.istc.pst.exploraa.api.Lesson;
import it.cnr.istc.pst.exploraa.api.Message;
import it.cnr.istc.pst.exploraa.server.db.LessonModelEntity;

/**
 * LessonManager
 */
public class LessonManager {

    static final Logger LOG = LoggerFactory.getLogger(LessonManager.class);
    private final CoreDeserializer deserializer = new CoreDeserializer();
    private final Gson gson = new GsonBuilder().registerTypeAdapter(Core.class, deserializer).create();
    private final Lesson lesson;
    private final String model;
    private final Solver solver = new Solver();
    private Core core;

    public LessonManager(final Lesson lesson) {
        this.lesson = lesson;

        final EntityManager em = App.EMF.createEntityManager();
        final LessonModelEntity model_entity = em.find(LessonModelEntity.class, lesson.getModelId());
        this.model = model_entity.getModel();
        em.close();
    }

    public void solve() {
        solver.read(model);
        solver.solve();
        final String state = solver.getState();

        core = new Core();
        core.read(model);
        deserializer.setCore(core);
        gson.fromJson(new JsonReader(new StringReader(state)), Core.class);

        // we collect the atoms from the solution..
        Map<Item, Collection<Atom>> atoms = new IdentityHashMap<>();
        for (Type t : core.getTypes().values()) {
            if (t.getName().equals("Lesson")) {
                t.getInstances().forEach(i -> atoms.put(i, new ArrayList<>()));
                for (Predicate p : t.getPredicates().values())
                    p.getInstances().stream().map(atm -> (Atom) atm)
                            .filter(atm -> (atm.getState() == Atom.AtomState.Active)).forEach(atm -> {
                                Item tau = atm.getTau();
                                if (tau instanceof Item.EnumItem)
                                    for (Item val : ((Item.EnumItem) tau).getVals())
                                        atoms.get(val).add(atm);
                                else
                                    atoms.get(tau).add(atm);
                            });
            }
        }

        // we send the atoms to the teacher..
        for (Map.Entry<Item, Collection<Atom>> i_atoms : atoms.entrySet())
            for (Atom atom : i_atoms.getValue()) {
                try {
                    MessagingService.getInstance().publish(lesson.getTeacher().getUser().getId() + "/input",
                            App.MAPPER.writeValueAsString(new Message.Token(lesson.getId(),
                                    ((Item.ArithItem) atom.getExpr("id")).getValue().intValue(),
                                    ((Item.ArithItem) atom.getExpr("start")).getValue().longValue())),
                            false);
                } catch (JsonProcessingException e) {
                    LOG.error("Failed at serializing token..", e);
                }
            }
    }
}