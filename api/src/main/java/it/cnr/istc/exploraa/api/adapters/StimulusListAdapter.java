package it.cnr.istc.exploraa.api.adapters;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonToken;
import com.google.gson.stream.JsonWriter;

import it.cnr.istc.exploraa.api.Message;

/**
 * StimulusListAdapter
 */
public class StimulusListAdapter extends TypeAdapter<List<Message.Stimulus>> {

    @Override
    public void write(JsonWriter out, List<Message.Stimulus> value) throws IOException {
        out.beginArray();
        for (Message.Stimulus s : value)
            Message.ADAPTER.write(out, s);
        out.endArray();
    }

    @Override
    public List<Message.Stimulus> read(JsonReader in) throws IOException {
        List<Message.Stimulus> stimuli = new ArrayList<>();
        in.beginArray();
        while (in.peek() != JsonToken.END_ARRAY)
            stimuli.add((Message.Stimulus) Message.ADAPTER.read(in));
        in.endArray();
        return stimuli;
    }
}