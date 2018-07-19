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
        LostParameter,
        Token,
        Stimulus;
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

    public static class LostParameter extends Message {

        public Parameter parameter;

        public LostParameter() {
        }

        public LostParameter(Parameter parameter) {
            super(MessageType.NewParameter);
            this.parameter = parameter;
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
            TextEvent, QuestionEvent, URLEvent
        }
    }
}
