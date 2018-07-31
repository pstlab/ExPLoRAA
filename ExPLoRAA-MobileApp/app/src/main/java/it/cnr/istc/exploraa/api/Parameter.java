/*
 * Copyright (C) 2018 Riccardo De Benedictis
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package it.cnr.istc.exploraa.api;

import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonToken;
import com.google.gson.stream.JsonWriter;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

/**
 * @author Riccardo De Benedictis
 */
public class Parameter {

    public static final ParameterAdapter ADAPTER = new ParameterAdapter();
    public String name;
    public Map<String, String> properties;

    public Parameter() {
    }

    public Parameter(String name, Map<String, String> properties) {
        this.name = name;
        this.properties = properties;
    }

    public static class ParameterAdapter extends TypeAdapter<Parameter> {

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
            return p;
        }
    }
}
