package it.cnr.istc.pst.exploraa.api;

import java.util.Collections;
import java.util.List;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.annotation.JsonSubTypes.Type;

/**
 * This is the base class for representing messages.
 */
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property = "type")
@JsonSubTypes({ @Type(value = Message.NewParameter.class, name = "new-parameter"),
        @Type(value = Message.RemoveParameter.class, name = "remove-parameter"),
        @Type(value = Message.FollowLesson.class, name = "follow-lesson"),
        @Type(value = Message.UnfollowLesson.class, name = "unfollow-lesson"),
        @Type(value = Message.RemoveLesson.class, name = "remove-lesson") })
public abstract class Message {

    /**
     * This message is used for communicating the creation of a new parameter.
     */
    public static class NewParameter extends Message {

        private final Parameter parameter;

        /**
         * @return the created parameter.
         */
        public Parameter getParameter() {
            return parameter;
        }

        @JsonCreator
        public NewParameter(@JsonProperty("parameter") Parameter parameter) {
            this.parameter = parameter;
        }
    }

    /**
     * This message is used for communicating the removal of an existing parameter.
     */
    public static class RemoveParameter extends Message {

        private final String parameter;

        /**
         * @return the name of the removed parameter.
         */
        public String getParameter() {
            return parameter;
        }

        @JsonCreator
        public RemoveParameter(@JsonProperty("parameter") String parameter) {
            this.parameter = parameter;
        }
    }

    /**
     * This message is used for communicating that a student is following a lesson.
     */
    public static class FollowLesson extends Message {

        private final User student;
        private final long lesson;
        private final Set<String> interests; // interests are here to allow their definition within the lesson model..

        @JsonCreator
        public FollowLesson(@JsonProperty("student") User student, @JsonProperty("lesson") long lesson,
                @JsonProperty("interests") Set<String> interests) {
            this.student = student;
            this.lesson = lesson;
            this.interests = interests;
        }

        /**
         * @return the following student.
         */
        public User getStudent() {
            return student;
        }

        /**
         * @return the followed lesson.
         */
        public long getLesson() {
            return lesson;
        }

        /**
         * @return the user's interests.
         */
        public Set<String> getInterests() {
            return Collections.unmodifiableSet(interests);
        }
    }

    /**
     * This message is used for communicating that a student is not following a
     * lesson anymore.
     */
    public static class UnfollowLesson extends Message {

        private final long student;
        private final long lesson;

        @JsonCreator
        public UnfollowLesson(@JsonProperty("student") long student, @JsonProperty("lesson") long lesson) {
            this.student = student;
            this.lesson = lesson;
        }

        /**
         * @return the unfollowing student.
         */
        public long getStudent() {
            return student;
        }

        /**
         * @return the unfollowed lesson.
         */
        public long getLesson() {
            return lesson;
        }
    }

    /**
     * This message is used for communicating that a lesson has been removed.
     */
    public static class RemoveLesson extends Message {

        private final long lesson;

        @JsonCreator
        public RemoveLesson(@JsonProperty("lesson") long lesson) {
            this.lesson = lesson;
        }

        /**
         * @return the removed lesson.
         */
        public long getLesson() {
            return lesson;
        }
    }

    /**
     * This message is used for communicating the creation of a new token within a
     * lesson.
     */
    public static class Token extends Message {

        private final long lesson_id;
        private final int id;
        private final Integer cause;
        private Long min, max;
        private long time;
        private final String refEvent;

        @JsonCreator
        public Token(@JsonProperty("lessonId") long lesson_id, @JsonProperty("id") int id,
                @JsonProperty("cause") Integer cause, @JsonProperty("min") Long min, @JsonProperty("max") Long max,
                @JsonProperty("time") long time, @JsonProperty("refEvent") String refEvent) {
            this.lesson_id = lesson_id;
            this.id = id;
            this.cause = cause;
            this.min = min;
            this.max = max;
            this.time = time;
            this.refEvent = refEvent;
        }

        /**
         * @return the lesson id
         */
        public long getLessonId() {
            return lesson_id;
        }

        /**
         * @return the id
         */
        public int getId() {
            return id;
        }

        /**
         * @return the cause
         */
        public Integer getCause() {
            return cause;
        }

        /**
         * @return the min
         */
        public Long getMin() {
            return min;
        }

        /**
         * @return the max
         */
        public Long getMax() {
            return max;
        }

        /**
         * @return the time
         */
        public long getTime() {
            return time;
        }

        /**
         * @return the refEvent
         */
        public String getRefEvent() {
            return refEvent;
        }
    }

    /**
     * This message is used for communicating the update of an existing token within
     * a lesson.
     */
    public static class TokenUpdate extends Message {

        private final long lesson_id;
        private final int id;
        private final Long min, max;
        private final long time;

        @JsonCreator
        public TokenUpdate(@JsonProperty("lessonId") long lesson_id, @JsonProperty("id") int id,
                @JsonProperty("min") Long min, @JsonProperty("max") Long max, @JsonProperty("time") long time) {
            this.lesson_id = lesson_id;
            this.id = id;
            this.min = min;
            this.max = max;
            this.time = time;
        }

        /**
         * @return the lesson id
         */
        public long getLessonId() {
            return lesson_id;
        }

        /**
         * @return the id
         */
        public int getId() {
            return id;
        }

        /**
         * @return the min
         */
        public Long getMin() {
            return min;
        }

        /**
         * @return the max
         */
        public Long getMax() {
            return max;
        }

        /**
         * @return the time
         */
        public long getTime() {
            return time;
        }
    }

    /**
     * This message is used for communicating the removal of an existing token from
     * a lesson.
     */
    public static class RemoveToken extends Message {

        private final long lesson_id;
        private final int id;

        @JsonCreator
        public RemoveToken(@JsonProperty("lessonId") long lesson_id, @JsonProperty("id") int id) {
            this.lesson_id = lesson_id;
            this.id = id;
        }

        /**
         * @return the lesson id
         */
        public long getLessonId() {
            return lesson_id;
        }

        /**
         * @return the id
         */
        public int getId() {
            return id;
        }
    }

    /**
     * This message is used for communicating the creation of a new stimulus. This
     * is the base class for representing stimuli.
     */
    @JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property = "stimulus-type")
    @JsonSubTypes({ @Type(value = Message.Stimulus.TextStimulus.class, name = "text-stimulus"),
            @Type(value = Message.Stimulus.QuestionStimulus.class, name = "question-stimulus"),
            @Type(value = Message.Stimulus.URLStimulus.class, name = "url-stimulus") })
    public abstract static class Stimulus extends Message {

        private final long lesson_id;
        private final int id;
        private final long time;

        @JsonCreator
        public Stimulus(@JsonProperty("lessonId") long lesson_id, @JsonProperty("id") int id,
                @JsonProperty("time") long time) {
            this.lesson_id = lesson_id;
            this.id = id;
            this.time = time;
        }

        /**
         * @return the lesson id
         */
        public long getLesson_id() {
            return lesson_id;
        }

        /**
         * @return the id
         */
        public int getId() {
            return id;
        }

        /**
         * @return the time
         */
        public long getTime() {
            return time;
        }

        /**
         * This message is used for communicating the creation of a new text stimulus.
         */
        public static class TextStimulus extends Stimulus {

            private final String content;

            @JsonCreator
            public TextStimulus(@JsonProperty("lessonId") long lesson_id, @JsonProperty("id") int id,
                    @JsonProperty("time") long time, @JsonProperty("content") String content) {
                super(lesson_id, id, time);
                this.content = content;
            }

            /**
             * @return the content
             */
            public String getContent() {
                return content;
            }
        }

        /**
         * This message is used for communicating the creation of a new question
         * stimulus.
         */
        public static class QuestionStimulus extends Stimulus {

            private final String question;
            private final List<String> answers;
            private final Integer answer;

            @JsonCreator
            public QuestionStimulus(@JsonProperty("lessonId") long lesson_id, @JsonProperty("id") int id,
                    @JsonProperty("time") long time, @JsonProperty("question") String question,
                    @JsonProperty("answers") List<String> answers, @JsonProperty("answer") Integer answer) {
                super(lesson_id, id, time);
                this.question = question;
                this.answers = answers;
                this.answer = answer;
            }

            /**
             * @return the question
             */
            public String getQuestion() {
                return question;
            }

            /**
             * @return the answers
             */
            public List<String> getAnswers() {
                return Collections.unmodifiableList(answers);
            }

            /**
             * @return the answer
             */
            public Integer getAnswer() {
                return answer;
            }

            public static class Answer extends Message {

                private final long lesson_id;
                private final int question_id;
                private final int answer;

                @JsonCreator
                public Answer(@JsonProperty("lessonId") long lesson_id, @JsonProperty("question_id") int question_id,
                        @JsonProperty("answer") int answer) {
                    this.lesson_id = lesson_id;
                    this.question_id = question_id;
                    this.answer = answer;
                }

                /**
                 * @return the lesson id
                 */
                public long getLessonId() {
                    return lesson_id;
                }

                /**
                 * @return the question id
                 */
                public int getQuestionId() {
                    return question_id;
                }

                /**
                 * @return the answer
                 */
                public int getAnswer() {
                    return answer;
                }
            }
        }

        /**
         * This message is used for communicating the creation of a new url stimulus.
         */
        public static class URLStimulus extends Stimulus {

            private final String content;
            private final String url;

            @JsonCreator
            public URLStimulus(@JsonProperty("lessonId") long lesson_id, @JsonProperty("id") int id,
                    @JsonProperty("time") long time, @JsonProperty("content") String content,
                    @JsonProperty("url") String url) {
                super(lesson_id, id, time);
                this.content = content;
                this.url = url;
            }

            /**
             * @return the content
             */
            public String getContent() {
                return content;
            }

            /**
             * @return the url
             */
            public String getUrl() {
                return url;
            }
        }
    }

    /**
     * This message is used for communicating the removal of an existing stimulus
     * from a lesson.
     */
    public static class RemoveStimulus extends Message {

        private final long lesson_id;
        private final long id;

        @JsonCreator
        public RemoveStimulus(@JsonProperty("lessonId") long lesson_id, @JsonProperty("id") long id) {
            this.lesson_id = lesson_id;
            this.id = id;
        }

        /**
         * @return the lesson_id
         */
        public long getLessonId() {
            return lesson_id;
        }

        /**
         * @return the id
         */
        public long getId() {
            return id;
        }
    }
}