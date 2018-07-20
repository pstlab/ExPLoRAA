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
package it.cnr.istc.pst.exploraa.webapp;

/**
 *
 * @author Riccardo De Benedictis
 */
public interface LessonManagerListener {

    /**
     * Notifies the listener that a new token has been created.
     *
     * @param tk the token that has been created.
     */
    public void newToken(final LessonManager.SolverToken tk);

    /**
     * Notifies the listener that the value of a token's temporal variable has
     * changed.
     *
     * @param tk the token whose temporal variable has changed its value.
     */
    public void movedToken(final LessonManager.SolverToken tk);

    /**
     * Notifies the listener that a token has to be executed.
     *
     * @param tk the token that has to be executed.
     */
    public void executeToken(final LessonManager.SolverToken tk);

    /**
     * Notifies the listener that a token has to be hidden.
     *
     * @param tk the token that has to be hidden.
     */
    public void hideToken(final LessonManager.SolverToken tk);

    /**
     * Notifies the listener that a token has to be removed.
     *
     * @param tk the token that has to be removed.
     */
    public void removeToken(final LessonManager.SolverToken tk);

    /**
     * Notifies the listener that the current time has changed.
     *
     * @param time the new current time.
     */
    public void newTime(final long time);
}
