package it.cnr.istc.pst.exploraa.api;

import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.annotation.JsonSubTypes.Type;

/**
 * Message
 */
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property = "type")
@JsonSubTypes({ @Type(value = Message.NewParameter.class, name = "new-parameter"),
        @Type(value = Message.RemoveParameter.class, name = "remove-parameter") })
public abstract class Message {

    public static class NewParameter extends Message {

        private Parameter parameter;

        /**
         * @return the parameter
         */
        public Parameter getParameter() {
            return parameter;
        }
    }

    public static class RemoveParameter extends Message {

        private String parameter;

        /**
         * @return the parameter
         */
        public String getParameter() {
            return parameter;
        }
    }
}