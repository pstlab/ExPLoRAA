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

import java.util.Collection;
import java.util.List;
import javax.json.JsonObject;
import javax.json.bind.adapter.JsonbAdapter;

/**
 *
 * @author Riccardo De Benedictis
 */
public abstract class Message {

    public MessageType message_type;

    public Message() {
    }

    public Message(MessageType type) {
        this.message_type = type;
    }

    public enum MessageType {
        NewParameter,
        RemoveParameter,
        NewLesson,
        RemoveLesson,
        Token,
        TokenUpdate,
        RemoveToken,
        Stimulus,
        Answer,
        RemoveStimulus;
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

        public Parameter parameter;

        public RemoveParameter() {
        }

        public RemoveParameter(Parameter parameter) {
            super(MessageType.RemoveParameter);
            this.parameter = parameter;
        }
    }

    public static class NewLesson extends Message {

        public Lesson lesson;

        public NewLesson() {
        }

        public NewLesson(Lesson lesson) {
            super(MessageType.NewLesson);
            this.lesson = lesson;
        }
    }

    public static class RemoveLesson extends Message {

        public long lesson_id;

        public RemoveLesson() {
        }

        public RemoveLesson(long lesson_id) {
            super(MessageType.RemoveLesson);
            this.lesson_id = lesson_id;
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

        public Token(long lesson_id, int id, Integer cause, Long min, Long max, long time, String refEvent, Integer question) {
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

        public TokenUpdate(long lesson_id, int id, Long min, Long max, long val) {
            super(MessageType.TokenUpdate);
            this.lesson_id = lesson_id;
            this.id = id;
            this.min = min;
            this.max = max;
            this.time = val;
        }
    }

    public static class RemoveToken extends Message {

        public long lesson_id;
        public long event_id;

        public RemoveToken() {
        }

        public RemoveToken(long lesson_id, long event_id) {
            super(MessageType.RemoveToken);
            this.lesson_id = lesson_id;
            this.event_id = event_id;
        }
    }

    public abstract static class Stimulus extends Message {

        public StimulusType event_type;
        public long lesson_id;
        public int id;
        public Collection<Long> students;
        public long time;

        public Stimulus() {
        }

        public Stimulus(StimulusType event_type, long lesson_id, int event_id, Collection<Long> students, long time) {
            super(MessageType.Stimulus);
            this.event_type = event_type;
            this.lesson_id = lesson_id;
            this.id = event_id;
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

            public TextStimulus(long lesson_id, int event_id, Collection<Long> students, long time, String content) {
                super(StimulusType.Text, lesson_id, event_id, students, time);
                this.content = content;
            }
        }

        public static class QuestionStimulus extends Stimulus {

            public String question;
            public List<String> answers;
            public Integer answer;

            public QuestionStimulus() {
            }

            public QuestionStimulus(long lesson_id, int event_id, Collection<Long> students, long time, String question, List<String> answers, Integer answer) {
                super(StimulusType.Question, lesson_id, event_id, students, time);
                this.question = question;
                this.answers = answers;
                this.answer = answer;
            }

            public static class Answer extends Message {

                public long lessonId;
                public int questionId;
                public int answer;

                public Answer() {
                }

                public Answer(long lessonId, int questionId, int answer) {
                    super(MessageType.Answer);
                    this.lessonId = lessonId;
                    this.questionId = questionId;
                    this.answer = answer;
                }
            }
        }

        public static class URLStimulus extends Stimulus {

            public String content;
            public String url;

            public URLStimulus() {
            }

            public URLStimulus(long lesson_id, int event_id, Collection<Long> students, long time, String content, String url) {
                super(StimulusType.URL, lesson_id, event_id, students, time);
                this.content = content;
                this.url = url;
            }
        }
    }

    public static class RemoveStimulus extends Message {

        public long lesson_id;
        public long event_id;

        public RemoveStimulus() {
        }

        public RemoveStimulus(long lesson_id, long event_id) {
            super(MessageType.RemoveStimulus);
            this.lesson_id = lesson_id;
            this.event_id = event_id;
        }
    }

    public static class StimulusAdapter implements JsonbAdapter<Stimulus, JsonObject> {

        @Override
        public JsonObject adaptToJson(Stimulus obj) throws Exception {
            throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
        }

        @Override
        public Stimulus adaptFromJson(JsonObject obj) throws Exception {
            throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
        }
    }
}
