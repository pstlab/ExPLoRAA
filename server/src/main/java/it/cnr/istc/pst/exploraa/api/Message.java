package it.cnr.istc.pst.exploraa.api;

import com.fasterxml.jackson.annotation.JsonTypeInfo;

import java.util.Collections;
import java.util.List;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonSubTypes.Type;

@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property = "type")
@JsonSubTypes({ @Type(value = Message.Online.class, name = "online"),
        @Type(value = Message.Follower.class, name = "follower"),
        @Type(value = Message.ProfileUpdate.class, name = "profile-update"),
        @Type(value = Message.FollowLesson.class, name = "follow-lesson"),
        @Type(value = Message.UnfollowLesson.class, name = "unfollow-lesson"),
        @Type(value = Message.RemoveLesson.class, name = "remove-lesson") })
public abstract class Message {

    /**
     * This message is used for communicating that a user is now online/offline.
     */
    public static class Online extends Message {

        private final long user;
        private final boolean online;

        @JsonCreator
        public Online(@JsonProperty("user") final long user, @JsonProperty("online") final boolean online) {
            this.user = user;
            this.online = online;
        }

        /**
         * @return the user.
         */
        public long getUser() {
            return user;
        }

        /**
         * @return the connected state.
         */
        public boolean isOnline() {
            return online;
        }
    }

    /**
     * This message is used for communicating to a teacher that a student has been
     * added/removed.
     */
    public static class Follower extends Message {

        private final long student;
        private final boolean added;

        @JsonCreator
        public Follower(@JsonProperty("student") final long student, @JsonProperty("added") final boolean added) {
            this.student = student;
            this.added = added;
        }

        /**
         * @return the student.
         */
        public long getStudent() {
            return student;
        }

        /**
         * @return the added state.
         */
        public boolean isAdded() {
            return added;
        }
    }

    /**
     * This message is used for communicating that a user profile has been updated.
     */
    public static class ProfileUpdate extends Message {

        private final long user;
        private final String profile;

        @JsonCreator
        public ProfileUpdate(@JsonProperty("user") long user, @JsonProperty("profile") String profile) {
            this.user = user;
            this.profile = profile;
        }

        public long getUser() {
            return user;
        }

        public String getProfile() {
            return profile;
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
        public FollowLesson(@JsonProperty("student") final User student, @JsonProperty("lesson") final long lesson,
                @JsonProperty("interests") final Set<String> interests) {
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
        public UnfollowLesson(@JsonProperty("student") final long student, @JsonProperty("lesson") final long lesson) {
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
        public RemoveLesson(@JsonProperty("lesson") final long lesson) {
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
        private final long id;
        private final long time;

        @JsonCreator
        public Token(@JsonProperty("lessonId") final long lesson_id, @JsonProperty("id") final long id,
                @JsonProperty("time") final long time) {
            this.lesson_id = lesson_id;
            this.id = id;
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
        public long getId() {
            return id;
        }

        /**
         * @return the time
         */
        public long getTime() {
            return time;
        }
    }

    /**
     * This message is used for communicating the update of an existing token within
     * a lesson.
     */
    public static class TokenUpdate extends Message {

        private final long lesson_id;
        private final long id;
        private final Long min, max;
        private final long time;

        @JsonCreator
        public TokenUpdate(@JsonProperty("lessonId") final long lesson_id, @JsonProperty("id") final long id,
                @JsonProperty("min") final Long min, @JsonProperty("max") final Long max,
                @JsonProperty("time") final long time) {
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
        public long getId() {
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
        private final long id;

        @JsonCreator
        public RemoveToken(@JsonProperty("lessonId") final long lesson_id, @JsonProperty("id") final long id) {
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
        public long getId() {
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
        private final long id;
        private final long time;
        private final boolean read;

        @JsonCreator
        public Stimulus(@JsonProperty("lessonId") final long lesson_id, @JsonProperty("id") final long id,
                @JsonProperty("time") final long time, @JsonProperty("read") final boolean read) {
            this.lesson_id = lesson_id;
            this.id = id;
            this.time = time;
            this.read = read;
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
        public long getId() {
            return id;
        }

        /**
         * @return the time
         */
        public long getTime() {
            return time;
        }

        /**
         * @return the read state
         */
        public boolean isRead() {
            return read;
        }

        /**
         * This message is used for communicating the creation of a new text stimulus.
         */
        public static class TextStimulus extends Stimulus {

            private final String content;

            @JsonCreator
            public TextStimulus(@JsonProperty("lessonId") final long lesson_id, @JsonProperty("id") final long id,
                    @JsonProperty("time") final long time, @JsonProperty("read") final boolean read,
                    @JsonProperty("content") final String content) {
                super(lesson_id, id, time, read);
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
            public QuestionStimulus(@JsonProperty("lessonId") final long lesson_id, @JsonProperty("id") final long id,
                    @JsonProperty("time") final long time, @JsonProperty("read") final boolean read,
                    @JsonProperty("question") final String question,
                    @JsonProperty("answers") final List<String> answers, @JsonProperty("answer") final Integer answer) {
                super(lesson_id, id, time, read);
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
                private final long question_id;
                private final long answer;

                @JsonCreator
                public Answer(@JsonProperty("lessonId") final long lesson_id,
                        @JsonProperty("question_id") final long question_id,
                        @JsonProperty("answer") final long answer) {
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
                public long getQuestionId() {
                    return question_id;
                }

                /**
                 * @return the answer
                 */
                public long getAnswer() {
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
            public URLStimulus(@JsonProperty("lessonId") final long lesson_id, @JsonProperty("id") final long id,
                    @JsonProperty("time") final long time, @JsonProperty("read") final boolean read,
                    @JsonProperty("content") final String content, @JsonProperty("url") final String url) {
                super(lesson_id, id, time, read);
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
        public RemoveStimulus(@JsonProperty("lessonId") final long lesson_id, @JsonProperty("id") final long id) {
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
