package it.cnr.istc.pst.exploraa.api;

import java.util.Collections;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * Parameter
 */
public class Parameter {

    private final String name;
    private final Map<String, String> properties;

    @JsonCreator
    public Parameter(@JsonProperty("name") String name, @JsonProperty("properties") Map<String, String> properties) {
        this.name = name;
        this.properties = properties;
    }

    /**
     * @return the name
     */
    public String getName() {
        return name;
    }

    /**
     * @return the properties
     */
    public Map<String, String> getProperties() {
        if (properties == null)
            return null;
        return Collections.unmodifiableMap(properties);
    }
}