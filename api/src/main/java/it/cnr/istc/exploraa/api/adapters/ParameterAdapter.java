package it.cnr.istc.exploraa.api.adapters;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonToken;
import com.google.gson.stream.JsonWriter;

import it.cnr.istc.exploraa.api.Parameter;

/**
 * ParameterAdapter
 */
public class ParameterAdapter extends TypeAdapter<Parameter> {

    @Override
    public void write(JsonWriter out, Parameter value) throws IOException {
        out.beginObject();
        out.name("name").value(value.name);
        out.name("properties");
        out.beginObject();
        for (Map.Entry<String, String> entry : value.properties.entrySet()) {
            out.name(entry.getKey()).value(entry.getValue());
        }
        out.endObject();
        out.endObject();
    }

    @Override
    public Parameter read(JsonReader in) throws IOException {
        Parameter p = new Parameter();
        in.beginObject();
        while (in.hasNext())
            switch ((in.nextName())) {
            case "name":
                p.name = in.nextString();
                break;
            case "properties":
                p.properties = new HashMap<>();
                in.beginObject();
                while (in.peek() != JsonToken.END_OBJECT)
                    p.properties.put(in.nextName(), in.nextString());
                in.endObject();
            }
        in.endObject();
        return p;
    }
}