package it.cnr.istc.exploraa.api;

import java.util.Map;

import com.google.gson.annotations.JsonAdapter;

import it.cnr.istc.exploraa.api.adapters.ParameterAdapter;

/**
 * Parameter
 */
@JsonAdapter(ParameterAdapter.class)
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
}