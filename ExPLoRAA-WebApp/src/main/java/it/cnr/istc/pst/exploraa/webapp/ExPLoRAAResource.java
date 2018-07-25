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

import it.cnr.istc.pst.exploraa.api.ExPLoRAA;
import it.cnr.istc.pst.exploraa.api.Follow;
import it.cnr.istc.pst.exploraa.api.Lesson;
import it.cnr.istc.pst.exploraa.api.Teach;
import it.cnr.istc.pst.exploraa.api.User;
import it.cnr.istc.pst.exploraa.webapp.db.FollowEntity;
import it.cnr.istc.pst.exploraa.webapp.db.TeachEntity;
import it.cnr.istc.pst.exploraa.webapp.db.UserEntity;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.annotation.Resource;
import javax.ejb.EJB;
import javax.json.bind.Jsonb;
import javax.json.bind.JsonbBuilder;
import javax.persistence.EntityManager;
import javax.persistence.NoResultException;
import javax.persistence.PersistenceContext;
import javax.persistence.TypedQuery;
import javax.transaction.HeuristicMixedException;
import javax.transaction.HeuristicRollbackException;
import javax.transaction.NotSupportedException;
import javax.transaction.RollbackException;
import javax.transaction.SystemException;
import javax.transaction.UserTransaction;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.MediaType;

/**
 *
 * @author Riccardo De Benedictis
 */
@Path("/")
public class ExPLoRAAResource implements ExPLoRAA {

    private static final Logger LOG = Logger.getLogger(ExPLoRAAResource.class.getName());
    private static final Jsonb JSONB = JsonbBuilder.create();
    @PersistenceContext
    private EntityManager em;
    @Resource
    private UserTransaction utx;
    @EJB
    private ExPLoRAABean ctx;

    @POST
    @Path("login")
    @Produces(MediaType.APPLICATION_JSON)
    @Override
    public User login(@FormParam("email") String email, @FormParam("password") String password) {
        LOG.log(Level.INFO, "Logging user: {0}", email);
        try {
            TypedQuery<UserEntity> query = em.createQuery("SELECT u FROM UserEntity u WHERE u.email = :email AND u.password = :password", UserEntity.class);
            query.setParameter("email", email);
            query.setParameter("password", password);
            UserEntity ue = query.getSingleResult();
            Map<Long, Follow> follows = new HashMap<>();
            for (FollowEntity follow : ue.getFollowedLessons()) {
                User teacher = new User(follow.getLesson().getTeachedBy().getTeacher().getId(), follow.getLesson().getTeachedBy().getTeacher().getEmail(), follow.getLesson().getTeachedBy().getTeacher().getFirstName(), follow.getLesson().getTeachedBy().getTeacher().getLastName(), ctx.isOnline(follow.getLesson().getTeachedBy().getTeacher().getId()), null, null, null, null);
                Lesson l = ctx.getLessonManager(follow.getLesson().getId()).getLesson();
                Lesson followed_lesson = new Lesson(follow.getLesson().getId(), new Teach(teacher, null), follow.getLesson().getName(), l.state, l.time, null, null, l.stimuli, null);
                follows.put(followed_lesson.id, new Follow(teacher, followed_lesson, follow.getInterests()));
            }
            Map<Long, Teach> teachs = new HashMap<>();
            for (TeachEntity teach : ue.getTeachedLessons()) {
                Lesson l = ctx.getLessonManager(teach.getLesson().getId()).getLesson();
                Map<Long, Follow> students = new HashMap<>();
                for (FollowEntity student : teach.getLesson().getStudents()) {
                    students.put(student.getStudent().getId(), new Follow(new User(student.getStudent().getId(), student.getStudent().getEmail(), student.getStudent().getFirstName(), student.getStudent().getLastName(), ctx.isOnline(student.getStudent().getId()), ctx.getParTypes(student.getStudent().getId()), ctx.getParValues(student.getStudent().getId()), null, null), null, student.getInterests()));
                }
                teachs.put(teach.getLesson().getId(), new Teach(null, new Lesson(teach.getLesson().getId(), null, teach.getLesson().getName(), l.state, l.time, l.model, students, null, l.tokens)));
            }
            return new User(ue.getId(), ue.getEmail(), ue.getFirstName(), ue.getLastName(), ctx.isOnline(ue.getId()), ctx.getParTypes(ue.getId()), ctx.getParValues(ue.getId()), follows, teachs);
        } catch (NoResultException ex) {
            throw new WebApplicationException(ex.getMessage());
        }
    }

    @POST
    @Path("new_user")
    @Produces(MediaType.APPLICATION_JSON)
    @Override
    public User new_user(@FormParam("email") String email, @FormParam("password") String password, @FormParam("first_name") String first_name, @FormParam("last_name") String last_name) {
        LOG.log(Level.INFO, "Creating new user: {0}", email);
        try {
            utx.begin();
            UserEntity u = new UserEntity();
            u.setEmail(email);
            u.setPassword(password);
            u.setFirstName(first_name);
            u.setLastName(last_name);
            em.persist(u);
            ctx.newUser(u.getId());
            utx.commit();
            return new User(u.getId(), u.getEmail(), u.getFirstName(), u.getLastName(), ctx.isOnline(u.getId()), null, null, null, null);
        } catch (IllegalStateException | SecurityException | HeuristicMixedException | HeuristicRollbackException | NotSupportedException | RollbackException | SystemException ex) {
            try {
                utx.rollback();
            } catch (IllegalStateException | SecurityException | SystemException ex1) {
                LOG.log(Level.SEVERE, null, ex1);
            }
            throw new WebApplicationException(ex.getMessage());
        }
    }

    @GET
    @Path("users")
    @Produces(MediaType.APPLICATION_JSON)
    @Override
    public Collection<User> getUsers() {
        LOG.info("retrieving all users..");
        List<UserEntity> user_entities = em.createQuery("SELECT u FROM UserEntity u", UserEntity.class).getResultList();
        Collection<User> users = new ArrayList<>();
        for (UserEntity ue : user_entities) {
            Map<Long, Follow> follows = new HashMap<>();
            for (FollowEntity follow : ue.getFollowedLessons()) {
                User teacher = new User(follow.getLesson().getTeachedBy().getTeacher().getId(), follow.getLesson().getTeachedBy().getTeacher().getEmail(), follow.getLesson().getTeachedBy().getTeacher().getFirstName(), follow.getLesson().getTeachedBy().getTeacher().getLastName(), ctx.isOnline(follow.getLesson().getTeachedBy().getTeacher().getId()), null, null, null, null);
                Lesson l = ctx.getLessonManager(follow.getLesson().getId()).getLesson();
                Lesson followed_lesson = new Lesson(follow.getLesson().getId(), new Teach(teacher, null), follow.getLesson().getName(), l.state, l.time, null, null, l.stimuli, null);
                follows.put(followed_lesson.id, new Follow(teacher, followed_lesson, follow.getInterests()));
            }
            Map<Long, Teach> teachs = new HashMap<>();
            for (TeachEntity teach : ue.getTeachedLessons()) {
                Lesson l = ctx.getLessonManager(teach.getLesson().getId()).getLesson();
                Map<Long, Follow> students = new HashMap<>();
                for (FollowEntity student : teach.getLesson().getStudents()) {
                    students.put(student.getStudent().getId(), new Follow(new User(student.getStudent().getId(), student.getStudent().getEmail(), student.getStudent().getFirstName(), student.getStudent().getLastName(), ctx.isOnline(student.getStudent().getId()), ctx.getParTypes(student.getStudent().getId()), ctx.getParValues(student.getStudent().getId()), null, null), null, student.getInterests()));
                }
                teachs.put(teach.getLesson().getId(), new Teach(null, new Lesson(teach.getLesson().getId(), null, teach.getLesson().getName(), l.state, l.time, l.model, students, null, l.tokens)));
            }
            users.add(new User(ue.getId(), ue.getEmail(), ue.getFirstName(), ue.getLastName(), ctx.isOnline(ue.getId()), ctx.getParTypes(ue.getId()), ctx.getParValues(ue.getId()), follows, teachs));
        }
        return users;
    }
}
