package it.cnr.istc.exploraa.api.adapters;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonToken;
import com.google.gson.stream.JsonWriter;

import it.cnr.istc.exploraa.api.LessonModel.StimulusTemplate;

/**
 * StimuliTemplateAdapter
 */
public class StimuliTemplateAdapter extends TypeAdapter<Map<String, StimulusTemplate>> {

    @Override
    public void write(JsonWriter out, Map<String, StimulusTemplate> value) throws IOException {
        out.beginArray();
        for (StimulusTemplate st : value.values())
            StimulusTemplate.ADAPTER.write(out, st);
        out.endArray();
    }

    @Override
    public Map<String, StimulusTemplate> read(JsonReader in) throws IOException {
        Map<String, StimulusTemplate> stimuli = new HashMap<>();
        in.beginArray();
        while (in.peek() != JsonToken.END_ARRAY) {
            StimulusTemplate st = StimulusTemplate.ADAPTER.read(in);
            stimuli.put(st.name, st);
        }
        in.endArray();
        return stimuli;
    }
}