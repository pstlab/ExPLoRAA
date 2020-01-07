package it.cnr.istc.pst.exploraa.api;

import java.util.Collections;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.annotation.JsonSubTypes.Type;

/**
 * Message
 */
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property = "type")
@JsonSubTypes({ @Type(value = Message.NewParameter.class, name = "new-parameter"),
        @Type(value = Message.RemoveParameter.class, name = "remove-parameter"),
        @Type(value = Message.FollowLesson.class, name = "follow-lesson"),
        @Type(value = Message.UnfollowLesson.class, name = "unfollow-lesson") })
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

    public static class FollowLesson extends Message {

        private User student;
        private long lesson;
        private Set<String> interests;

        /**
         * @return the student
         */
        public User getStudent() {
            return student;
        }

        /**
         * @return the lesson
         */
        public long getLesson() {
            return lesson;
        }

        /**
         * @return the interests
         */
        public Set<String> getInterests() {
            return Collections.unmodifiableSet(interests);
        }
    }

    public static class UnfollowLesson extends Message {

        private long student;
        private long lesson;

        /**
         * @return the student
         */
        public long getStudent() {
            return student;
        }

        /**
         * @return the lesson
         */
        public long getLesson() {
            return lesson;
        }
    }
}