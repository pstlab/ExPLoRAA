package it.cnr.istc.exploraa.api.adapters;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;

import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonToken;
import com.google.gson.stream.JsonWriter;

import it.cnr.istc.exploraa.api.LessonModel;
import it.cnr.istc.exploraa.api.LessonModel.Relation;
import it.cnr.istc.exploraa.api.LessonModel.StimulusTemplate;

/**
 * LessonModelAdapter
 */
public class LessonModelAdapter extends TypeAdapter<LessonModel> {

    @Override
    public void write(JsonWriter out, LessonModel value) throws IOException {
        out.beginObject();
        if (value.id != null)
            out.name("id").value(value.id);

        out.name("name").value(value.name);

        out.name("stimuli");
        out.beginArray();
        for (StimulusTemplate stimulus_template : value.stimuli.values())
            StimulusTemplate.ADAPTER.write(out, stimulus_template);
        out.endArray();

        out.name("ids");
        out.beginArray();
        for (String id : value.ids)
            out.value(id);
        out.endArray();

        out.name("relations");
        out.beginArray();
        for (Relation rel : value.relations)
            Relation.ADAPTER.write(out, rel);
        out.endArray();

        out.endObject();
    }

    @Override
    public LessonModel read(JsonReader in) throws IOException {
        LessonModel model = new LessonModel();
        in.beginObject();
        while (in.hasNext())
            switch ((in.nextName())) {
            case "id":
                model.id = in.nextLong();
                break;
            case "name":
                model.name = in.nextString();
                break;
            case "stimuli":
                model.stimuli = new HashMap<>();
                in.beginArray();
                while (in.peek() != JsonToken.END_ARRAY) {
                    StimulusTemplate st = StimulusTemplate.ADAPTER.read(in);
                    model.stimuli.put(st.name, st);
                }
                in.endArray();
                break;
            case "ids":
                model.ids = new HashSet<>();
                in.beginArray();
                while (in.peek() != JsonToken.END_ARRAY)
                    model.ids.add(in.nextString());
                in.endArray();
                break;
            case "relations":
                model.relations = new ArrayList<>();
                in.beginArray();
                while (in.peek() != JsonToken.END_ARRAY) {
                    Relation rel = Relation.ADAPTER.read(in);
                    model.relations.add(rel);
                }
                in.endArray();
                break;
            }
        in.endObject();
        return model;
    }
}