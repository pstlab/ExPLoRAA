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
package it.cnr.istc.pst.exploraa.api;

import java.util.HashMap;
import java.util.Map;
import javax.json.Json;
import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;
import javax.json.JsonString;
import javax.json.JsonValue;
import javax.json.bind.adapter.JsonbAdapter;

/**
 *
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

    public static class ParameterAdapter implements JsonbAdapter<Parameter, JsonObject> {

        @Override
        public JsonObject adaptToJson(Parameter obj) throws Exception {
            JsonObjectBuilder parameter_object = Json.createObjectBuilder();
            parameter_object.add("name", obj.name);
            JsonObjectBuilder properties_object = Json.createObjectBuilder();
            for (Map.Entry<String, String> entry : obj.properties.entrySet()) {
                properties_object.add(entry.getKey(), entry.getValue());
            }
            parameter_object.add("properties", properties_object);
            return parameter_object.build();
        }

        @Override
        public Parameter adaptFromJson(JsonObject obj) throws Exception {
            Map<String, String> properties = new HashMap<>();
            JsonObject properties_object = obj.getJsonObject("properties");
            for (Map.Entry<String, JsonValue> prop : properties_object.entrySet()) {
                properties.put(prop.getKey(), ((JsonString) prop.getValue()).getString());
            }
            return new Parameter(obj.getString("name"), properties);
        }
    }
}
