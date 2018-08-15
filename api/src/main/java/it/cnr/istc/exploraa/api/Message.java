package it.cnr.istc.exploraa.api;

import java.util.List;
import java.util.Set;

import com.google.gson.annotations.JsonAdapter;

import it.cnr.istc.exploraa.api.adapters.MessageAdapter;
import it.cnr.istc.exploraa.api.adapters.UserAdapter;

/**
 * Message
 */
@JsonAdapter(MessageAdapter.class)
public class Message {

    public static final MessageAdapter ADAPTER = new MessageAdapter();
    public MessageType message_type;

    public Message() {
    }

    public Message(MessageType type) {
        this.message_type = type;
    }

    public enum MessageType {
        NewParameter, RemoveParameter, FollowLesson, UnfollowLesson, RemoveLesson, Token, TokenUpdate, RemoveToken,
        Stimulus, Answer, RemoveStimulus
    }

    public static class NewParameter extends Message {

        public Parameter parameter;

        public NewParameter() {
        }

        public NewParameter(Parameter parameter) {
            super(MessageType.NewParameter);
            this.parameter = parameter;
        }
    }

    public static class RemoveParameter extends Message {

        public String parameter;

        public RemoveParameter() {
        }

        public RemoveParameter(String parameter) {
            super(MessageType.RemoveParameter);
            this.parameter = parameter;
        }
    }

    public static class FollowLesson extends Message {

        @JsonAdapter(UserAdapter.class)
        public User student;
        public long lesson;
        public Set<String> interests;

        public FollowLesson() {
        }

        public FollowLesson(User student, long lesson, Set<String> interests) {
            super(MessageType.FollowLesson);
            this.student = student;
            this.lesson = lesson;
            this.interests = interests;
        }
    }

    public static class UnfollowLesson extends Message {

        public long student;
        public long lesson;

        public UnfollowLesson() {
        }

        public UnfollowLesson(long student, long lesson) {
            super(MessageType.UnfollowLesson);
            this.student = student;
            this.lesson = lesson;
        }
    }

    public static class RemoveLesson extends Message {

        public long lesson;

        public RemoveLesson() {
        }

        public RemoveLesson(long lesson) {
            super(MessageType.RemoveLesson);
            this.lesson = lesson;
        }
    }

    public static class Token extends Message {

        public long lesson_id;
        public int id;
        public Integer cause;
        public Long min;
        public Long max;
        public long time;
        public String refEvent;
        public Integer question;

        public Token() {
        }

        public Token(long lesson_id, int id, Integer cause, Long min, Long max, long time, String refEvent,
                Integer question) {
            super(MessageType.Token);
            this.lesson_id = lesson_id;
            this.id = id;
            this.cause = cause;
            this.min = min;
            this.max = max;
            this.time = time;
            this.refEvent = refEvent;
            this.question = question;
        }
    }

    public static class TokenUpdate extends Message {

        public long lesson_id;
        public int id;
        public Long min, max;
        public long time;

        public TokenUpdate() {
        }

        public TokenUpdate(long lesson_id, int id, Long min, Long max, long time) {
            super(MessageType.TokenUpdate);
            this.lesson_id = lesson_id;
            this.id = id;
            this.min = min;
            this.max = max;
            this.time = time;
        }
    }

    public static class RemoveToken extends Message {

        public long lesson_id;
        public int id;

        public RemoveToken() {
        }

        public RemoveToken(long lesson_id, int id) {
            super(MessageType.RemoveToken);
            this.lesson_id = lesson_id;
            this.id = id;
        }
    }

    public abstract static class Stimulus extends Message {

        public StimulusType stimulus_type;
        public long lesson_id;
        public int id;
        public Set<Long> students;
        public long time;

        public Stimulus() {
        }

        public Stimulus(StimulusType stimulus_type, long lesson_id, int id, Set<Long> students, long time) {
            super(MessageType.Stimulus);
            this.stimulus_type = stimulus_type;
            this.lesson_id = lesson_id;
            this.id = id;
            this.students = students;
            this.time = time;
        }

        public enum StimulusType {
            Text, Question, URL
        }

        public static class TextStimulus extends Stimulus {

            public String content;

            public TextStimulus() {
            }

            public TextStimulus(long lesson_id, int id, Set<Long> students, long time, String content) {
                super(StimulusType.Text, lesson_id, id, students, time);
                this.content = content;
            }
        }

        public static class QuestionStimulus extends Stimulus {

            public String question;
            public List<String> answers;
            public Integer answer;

            public QuestionStimulus() {
            }

            public QuestionStimulus(long lesson_id, int id, Set<Long> students, long time, String question,
                    List<String> answers, Integer answer) {
                super(StimulusType.Question, lesson_id, id, students, time);
                this.question = question;
                this.answers = answers;
                this.answer = answer;
            }

            public static class Answer extends Message {

                public long lesson_id;
                public int question_id;
                public int answer;

                public Answer() {
                }

                public Answer(long lesson_id, int question_id, int answer) {
                    super(MessageType.Answer);
                    this.lesson_id = lesson_id;
                    this.question_id = question_id;
                    this.answer = answer;
                }
            }
        }

        public static class URLStimulus extends Stimulus {

            public String content;
            public String url;

            public URLStimulus() {
            }

            public URLStimulus(long lesson_id, int id, Set<Long> students, long time, String content, String url) {
                super(StimulusType.URL, lesson_id, id, students, time);
                this.content = content;
                this.url = url;
            }
        }
    }

    public static class RemoveStimulus extends Message {

        public long lesson_id;
        public long id;

        public RemoveStimulus() {
        }

        public RemoveStimulus(long lesson_id, long id) {
            super(MessageType.RemoveStimulus);
            this.lesson_id = lesson_id;
            this.id = id;
        }
    }
}