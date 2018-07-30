/*
 * Copyright (C) 2018 Your Organisation
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
package it.cnr.istc.pst.exploraa.desktopapp;

import javafx.util.StringConverter;

/**
 *
 * @author Riccardo De Benedictis
 */
public class TimeStringConverter extends StringConverter<Long> {

    @Override
    public String toString(Long time) {
        if (time == null) {
            return "";
        }
        long second = (time / 1000) % 60;
        long minute = (time / (1000 * 60)) % 60;
        long hour = (time / (1000 * 60 * 60)) % 24;
        long days = (time / (1000 * 60 * 60 * 24));
        if (days == 0) {
            if (hour == 0) {
                return String.format("%02d:%02d", minute, second);
            } else {
                return String.format("%02d:%02d:%02d", hour, minute, second);
            }
        } else {
            return String.format("%03d:%02d:%02d:%02d", days, hour, minute, second);
        }
    }

    @Override
    public Long fromString(String string) {
        String[] split = string.split(":");
        long seconds = 0;
        long minutes = 0;
        long hours = 0;
        long days = 0;
        switch (split.length) {
            case 2:
                seconds = Long.parseLong(split[1]);
                minutes = Long.parseLong(split[0]);
                break;
            case 3:
                seconds = Long.parseLong(split[2]);
                minutes = Long.parseLong(split[1]);
                hours = Long.parseLong(split[0]);
                break;
            case 4:
                seconds = Long.parseLong(split[3]);
                minutes = Long.parseLong(split[2]);
                hours = Long.parseLong(split[1]);
                days = Long.parseLong(split[0]);
                break;
            default:
                throw new AssertionError();
        }
        return seconds * 1000 + minutes * 1000 * 60 + hours * 1000 * 60 * 60 + days * 1000 * 60 * 60 * 24;
    }
}
