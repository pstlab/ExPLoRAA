package it.cnr.istc.exploraa.api.adapters;

import java.io.IOException;
import java.util.concurrent.TimeUnit;

import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

import it.cnr.istc.exploraa.api.LessonModel.Relation;

/**
 * RelationAdapter
 */
public class RelationAdapter extends TypeAdapter<Relation> {

    @Override
    public void write(JsonWriter out, Relation value) throws IOException {
        out.beginObject();
        out.name("from").value(value.from);
        out.name("to").value(value.to);
        if (value.lb != Double.NEGATIVE_INFINITY)
            out.name("lb").value(value.lb);
        if (value.ub != Double.NEGATIVE_INFINITY)
            out.name("ub").value(value.ub);
        out.name("unit").value(value.unit.name());
        out.endObject();
    }

    @Override
    public Relation read(JsonReader in) throws IOException {
        Relation rel = new Relation();
        in.beginObject();
        while (in.hasNext())
            switch ((in.nextName())) {
            case "from":
                rel.from = in.nextString();
                break;
            case "to":
                rel.to = in.nextString();
                break;
            case "lb":
                rel.lb = in.nextLong();
                break;
            case "ub":
                rel.ub = in.nextLong();
                break;
            case "unit":
                rel.unit = TimeUnit.valueOf(in.nextString());
                break;
            }
        in.endObject();
        return rel;
    }
}