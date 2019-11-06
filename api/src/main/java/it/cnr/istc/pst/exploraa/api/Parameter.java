package it.cnr.istc.pst.exploraa.api;

import java.util.Collections;
import java.util.Map;

/**
 * Parameter
 */
public class Parameter {

    private String name;
    private Map<String, String> properties;

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