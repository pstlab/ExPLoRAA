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
import it.cnr.istc.pst.exploraa.api.LessonModel;
import it.cnr.istc.pst.exploraa.api.Message;
import it.cnr.istc.pst.exploraa.api.Teach;
import it.cnr.istc.pst.exploraa.api.User;
import it.cnr.istc.pst.exploraa.webapp.db.FollowEntity;
import it.cnr.istc.pst.exploraa.webapp.db.FollowId;
import it.cnr.istc.pst.exploraa.webapp.db.LessonEntity;
import it.cnr.istc.pst.exploraa.webapp.db.LessonModelEntity;
import it.cnr.istc.pst.exploraa.webapp.db.TeachEntity;
import it.cnr.istc.pst.exploraa.webapp.db.UserEntity;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;
import javax.annotation.Resource;
import javax.ejb.EJB;
import javax.json.bind.JsonbException;
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
import javax.ws.rs.DELETE;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
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
        LOG.log(Level.INFO, "logging user {0}", email);
        try {
            TypedQuery<UserEntity> query = em.createQuery("SELECT u FROM UserEntity u WHERE u.email = :email AND u.password = :password", UserEntity.class);
            query.setParameter("email", email);
            query.setParameter("password", password);
            UserEntity ue = query.getSingleResult();
            Map<Long, Follow> follows = new HashMap<>();
            for (FollowEntity follow : ue.getFollowedLessons()) {
                User teacher = new User(follow.getLesson().getTeachedBy().getTeacher().getId(), follow.getLesson().getTeachedBy().getTeacher().getEmail(), follow.getLesson().getTeachedBy().getTeacher().getFirstName(), follow.getLesson().getTeachedBy().getTeacher().getLastName(), ctx.isOnline(follow.getLesson().getTeachedBy().getTeacher().getId()), null, null, null, null, null);
                Lesson l = ctx.getLessonManager(follow.getLesson().getId()).getLesson();
                // we filter those stimuli for which the student is interested..
                List<Message.Stimulus> stimului = l.stimuli.stream().filter(s -> s.students.contains(ue.getId())).collect(Collectors.toList());
                Lesson followed_lesson = new Lesson(follow.getLesson().getId(), follow.getLesson().getName(), null, l.topics, stimului, null, new Teach(teacher, null), null, l.state, l.time);
                follows.put(followed_lesson.id, new Follow(teacher, followed_lesson, new HashSet<>(follow.getInterests())));
            }
            Map<Long, Teach> teachs = new HashMap<>();
            for (TeachEntity teach : ue.getTeachedLessons()) {
                Lesson l = ctx.getLessonManager(teach.getLesson().getId()).getLesson();
                Map<Long, Follow> students = new HashMap<>();
                for (FollowEntity student : teach.getLesson().getStudents()) {
                    students.put(student.getStudent().getId(), new Follow(new User(student.getStudent().getId(), student.getStudent().getEmail(), student.getStudent().getFirstName(), student.getStudent().getLastName(), ctx.isOnline(student.getStudent().getId()), ctx.getParTypes(student.getStudent().getId()), ctx.getParValues(student.getStudent().getId()), null, null, null), null, new HashSet<>(student.getInterests())));
                }
                teachs.put(teach.getLesson().getId(), new Teach(null, new Lesson(teach.getLesson().getId(), teach.getLesson().getName(), l.model, null, null, l.tokens, null, students, l.state, l.time)));
            }
            Map<Long, LessonModel> models = ue.getModels().stream().map(model -> ExPLoRAABean.JSONB.fromJson(model.getModel(), LessonModel.class)).collect(Collectors.toMap(m -> m.id, m -> m));
            return new User(ue.getId(), ue.getEmail(), ue.getFirstName(), ue.getLastName(), ctx.isOnline(ue.getId()), ctx.getParTypes(ue.getId()), ctx.getParValues(ue.getId()), follows, teachs, models);
        } catch (NoResultException ex) {
            throw new WebApplicationException(ex.getMessage());
        }
    }

    @POST
    @Path("new_user")
    @Produces(MediaType.APPLICATION_JSON)
    @Override
    public User new_user(@FormParam("email") String email, @FormParam("password") String password, @FormParam("first_name") String first_name, @FormParam("last_name") String last_name) {
        LOG.log(Level.INFO, "creating new user {0}", email);
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
            return new User(u.getId(), u.getEmail(), u.getFirstName(), u.getLastName(), ctx.isOnline(u.getId()), null, null, null, null, null);
        } catch (IllegalStateException | SecurityException | HeuristicMixedException | HeuristicRollbackException | NotSupportedException | RollbackException | SystemException ex) {
            try {
                utx.rollback();
            } catch (IllegalStateException | SecurityException | SystemException ex1) {
                LOG.log(Level.SEVERE, null, ex1);
            }
            throw new WebApplicationException(ex.getMessage());
        }
    }

    @DELETE
    @Path("user/{id}")
    @Override
    public void delete_user(@PathParam("id") long id) {
        LOG.log(Level.INFO, "deleting user {0}", id);
        try {
            utx.begin();
            UserEntity u = em.find(UserEntity.class, id);
            for (FollowEntity f : u.getFollowedLessons()) {
                f.getLesson().removeStudent(f);
                em.merge(f.getLesson());
            }
            em.remove(u);
            ctx.deleteUser(id);
            utx.commit();
        } catch (NotSupportedException | SystemException | RollbackException | HeuristicMixedException | HeuristicRollbackException | SecurityException | IllegalStateException ex) {
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
    public Collection<User> get_users() {
        LOG.info("retrieving all users..");
        List<UserEntity> user_entities = em.createQuery("SELECT u FROM UserEntity u", UserEntity.class).getResultList();
        Collection<User> users = new ArrayList<>();
        for (UserEntity ue : user_entities) {
            Map<Long, Follow> follows = new HashMap<>();
            for (FollowEntity follow : ue.getFollowedLessons()) {
                User teacher = new User(follow.getLesson().getTeachedBy().getTeacher().getId(), follow.getLesson().getTeachedBy().getTeacher().getEmail(), follow.getLesson().getTeachedBy().getTeacher().getFirstName(), follow.getLesson().getTeachedBy().getTeacher().getLastName(), ctx.isOnline(follow.getLesson().getTeachedBy().getTeacher().getId()), null, null, null, null, null);
                Lesson l = ctx.getLessonManager(follow.getLesson().getId()).getLesson();
                Lesson followed_lesson = new Lesson(follow.getLesson().getId(), follow.getLesson().getName(), null, l.topics, l.stimuli.stream().filter(st -> st.students.contains(ue.getId())).collect(Collectors.toList()), null, new Teach(teacher, null), null, l.state, l.time);
                follows.put(followed_lesson.id, new Follow(teacher, followed_lesson, new HashSet<>(follow.getInterests())));
            }
            Map<Long, Teach> teachs = new HashMap<>();
            for (TeachEntity teach : ue.getTeachedLessons()) {
                Lesson l = ctx.getLessonManager(teach.getLesson().getId()).getLesson();
                Map<Long, Follow> students = new HashMap<>();
                for (FollowEntity student : teach.getLesson().getStudents()) {
                    students.put(student.getStudent().getId(), new Follow(new User(student.getStudent().getId(), student.getStudent().getEmail(), student.getStudent().getFirstName(), student.getStudent().getLastName(), ctx.isOnline(student.getStudent().getId()), ctx.getParTypes(student.getStudent().getId()), ctx.getParValues(student.getStudent().getId()), null, null, null), null, new HashSet<>(student.getInterests())));
                }
                teachs.put(teach.getLesson().getId(), new Teach(null, new Lesson(teach.getLesson().getId(), teach.getLesson().getName(), l.model, null, null, l.tokens, null, students, l.state, l.time)));
            }
            Map<Long, LessonModel> models = ue.getModels().stream().map(model -> ExPLoRAABean.JSONB.fromJson(model.getModel(), LessonModel.class)).collect(Collectors.toMap(m -> m.id, m -> m));
            users.add(new User(ue.getId(), ue.getEmail(), ue.getFirstName(), ue.getLastName(), ctx.isOnline(ue.getId()), ctx.getParTypes(ue.getId()), ctx.getParValues(ue.getId()), follows, teachs, models));
        }
        LOG.log(Level.INFO, "found {0} users", users.size());
        return users;
    }

    @POST
    @Path("new_lesson_by_model")
    @Produces(MediaType.APPLICATION_JSON)
    @Override
    public Lesson new_lesson(@FormParam("teacher_id") long teacher_id, @FormParam("name") String name, @FormParam("model") String model) {
        LOG.info("creating a new lesson by new model..");
        try {
            utx.begin();
            UserEntity teacher_entity = em.find(UserEntity.class, teacher_id);
            LessonModelEntity model_entity = new LessonModelEntity();
            em.persist(model_entity);

            // we set the id of the model..
            LessonModel c_model = ExPLoRAABean.JSONB.fromJson(model, LessonModel.class);
            c_model.id = model_entity.getId();
            model_entity.setModel(ExPLoRAABean.JSONB.toJson(c_model));
            em.merge(model_entity);

            teacher_entity.addModel(model_entity);
            LessonEntity lesson_entity = new LessonEntity();
            lesson_entity.setName(name);
            lesson_entity.setModel(model_entity);
            em.persist(lesson_entity);

            TeachEntity te = new TeachEntity(teacher_entity, lesson_entity);
            em.persist(te);

            lesson_entity.setTeachedBy(te);
            em.merge(lesson_entity);

            teacher_entity.addTeachedLesson(te);
            em.merge(teacher_entity);

            Set<String> topics = new HashSet<>();
            for (LessonModel.StimulusTemplate template : c_model.stimuli.values()) {
                topics.addAll(template.topics);
            }

            Lesson l = new Lesson(lesson_entity.getId(), name, c_model, topics, new ArrayList<>(), new ArrayList<>(), new Teach(new User(teacher_id, teacher_entity.getEmail(), teacher_entity.getFirstName(), teacher_entity.getLastName(), ctx.isOnline(teacher_id), null, null, null, null, null), null), new HashMap<>(), Lesson.LessonState.Stopped, 0);
            ctx.newLesson(l);

            utx.commit();
            return new Lesson(l.id, name, null, topics, null, null, null, null, l.state, l.time);
        } catch (NotSupportedException | SystemException | RollbackException | HeuristicMixedException | HeuristicRollbackException | SecurityException | IllegalStateException ex) {
            try {
                utx.rollback();
            } catch (IllegalStateException | SecurityException | SystemException ex1) {
                LOG.log(Level.SEVERE, null, ex1);
            }
            throw new WebApplicationException(ex.getMessage());
        }
    }

    @POST
    @Path("new_lesson_by_model_id")
    @Produces(MediaType.APPLICATION_JSON)
    @Override
    public Lesson new_lesson(@FormParam("teacher_id") long teacher_id, @FormParam("name") String name, @FormParam("model_id") long model_id) {
        LOG.info("creating a new lesson by existing model..");
        try {
            utx.begin();
            UserEntity teacher_entity = em.find(UserEntity.class, teacher_id);
            LessonModelEntity model_entity = em.find(LessonModelEntity.class, model_id);
            LessonEntity lesson_entity = new LessonEntity();
            lesson_entity.setName(name);
            lesson_entity.setModel(model_entity);
            em.persist(lesson_entity);

            TeachEntity te = new TeachEntity(teacher_entity, lesson_entity);
            em.persist(te);

            lesson_entity.setTeachedBy(te);
            em.merge(lesson_entity);

            teacher_entity.addTeachedLesson(te);
            em.merge(teacher_entity);

            LessonModel c_model = ExPLoRAABean.JSONB.fromJson(model_entity.getModel(), LessonModel.class);
            Set<String> topics = new HashSet<>();
            for (LessonModel.StimulusTemplate template : c_model.stimuli.values()) {
                topics.addAll(template.topics);
            }

            Lesson l = new Lesson(lesson_entity.getId(), name, c_model, topics, new ArrayList<>(), new ArrayList<>(), new Teach(new User(teacher_id, teacher_entity.getEmail(), teacher_entity.getFirstName(), teacher_entity.getLastName(), ctx.isOnline(teacher_id), null, null, null, null, null), null), new HashMap<>(), Lesson.LessonState.Stopped, 0);
            ctx.newLesson(l);

            utx.commit();
            return new Lesson(l.id, name, null, topics, null, null, null, null, l.state, l.time);
        } catch (NotSupportedException | SystemException | RollbackException | HeuristicMixedException | HeuristicRollbackException | SecurityException | IllegalStateException ex) {
            try {
                utx.rollback();
            } catch (IllegalStateException | SecurityException | SystemException ex1) {
                LOG.log(Level.SEVERE, null, ex1);
            }
            throw new WebApplicationException(ex.getMessage());
        }
    }

    @DELETE
    @Path("lesson/{id}")
    @Override
    public void delete_lesson(@PathParam("id") long id) {
        LOG.log(Level.INFO, "deleting lesson {0}", id);
        try {
            utx.begin();
            LessonEntity lesson = em.find(LessonEntity.class, id);
            ctx.removeLesson(id);
            lesson.getTeachedBy().getTeacher().removeTeachedLesson(lesson.getTeachedBy());
            em.merge(lesson.getTeachedBy().getTeacher());
            for (FollowEntity f : lesson.getStudents()) {
                f.getStudent().removeFollowedLesson(f);
                em.merge(f.getStudent());
            }
            em.remove(lesson);
            utx.commit();
        } catch (NotSupportedException | SystemException | RollbackException | HeuristicMixedException | HeuristicRollbackException | SecurityException | IllegalStateException ex) {
            try {
                utx.rollback();
            } catch (IllegalStateException | SecurityException | SystemException ex1) {
                LOG.log(Level.SEVERE, null, ex1);
            }
            throw new WebApplicationException(ex.getMessage());
        }
    }

    @GET
    @Path("lessons")
    @Produces(MediaType.APPLICATION_JSON)
    @Override
    public Collection<Lesson> get_lessons() {
        LOG.info("retrieving all lessons..");
        List<Lesson> lessons = ctx.getLessonManagers().stream().map(lm -> lm.getLesson()).map(l -> new Lesson(l.id, l.name, l.model, l.topics, null, null, new Teach(l.teacher.user, null), l.students.values().stream().collect(Collectors.toMap(s -> s.user.id, s -> new Follow(s.user, null, s.interests))), l.state, l.time)).collect(Collectors.toList());
        LOG.log(Level.INFO, "found {0} lessons", lessons.size());
        return lessons;
    }

    @PUT
    @Path("follow")
    @Produces(MediaType.APPLICATION_JSON)
    @Override
    public Lesson follow(@FormParam("user_id") long user_id, @FormParam("lesson_id") long lesson_id, @FormParam("interests") String interests) {
        LOG.log(Level.INFO, "User {0} is trying to follow lesson {1} with interests {2}", new Object[]{user_id, lesson_id, interests});
        try {
            utx.begin();
            UserEntity student = em.find(UserEntity.class, user_id);
            LessonEntity lesson = em.find(LessonEntity.class, lesson_id);
            ArrayList<String> c_interests = ExPLoRAABean.JSONB.fromJson(interests, new ArrayList<String>() {
            }.getClass().getGenericSuperclass());

            FollowEntity follow = new FollowEntity();
            follow.setStudent(student);
            follow.setLesson(lesson);
            for (String c_interest : c_interests) {
                follow.addInterest(c_interest);
            }
            em.persist(follow);

            student.addFollowedLesson(follow);
            em.merge(student);
            lesson.addStudent(follow);
            em.merge(lesson);

            ctx.follow(new User(student.getId(), student.getEmail(), student.getFirstName(), student.getLastName(), ctx.isOnline(student.getId()), ctx.getParTypes(student.getId()), ctx.getParValues(student.getId()), null, null, null), lesson_id, new HashSet<>(c_interests));
            utx.commit();

            User teacher = new User(follow.getLesson().getTeachedBy().getTeacher().getId(), follow.getLesson().getTeachedBy().getTeacher().getEmail(), follow.getLesson().getTeachedBy().getTeacher().getFirstName(), follow.getLesson().getTeachedBy().getTeacher().getLastName(), ctx.isOnline(follow.getLesson().getTeachedBy().getTeacher().getId()), null, null, null, null, null);
            Lesson l = ctx.getLessonManager(follow.getLesson().getId()).getLesson();
            return new Lesson(follow.getLesson().getId(), follow.getLesson().getName(), null, l.topics, l.stimuli.stream().filter(st -> st.students.contains(student.getId())).collect(Collectors.toList()), null, new Teach(teacher, null), null, l.state, l.time);
        } catch (IllegalStateException | SecurityException | JsonbException | HeuristicMixedException | HeuristicRollbackException | NotSupportedException | RollbackException | SystemException ex) {
            try {
                utx.rollback();
            } catch (IllegalStateException | SecurityException | SystemException ex1) {
                LOG.log(Level.SEVERE, null, ex1);
            }
            throw new WebApplicationException(ex.getMessage());
        }
    }

    @PUT
    @Path("unfollow")
    @Override
    public void unfollow(@FormParam("user_id") long user_id, @FormParam("lesson_id") long lesson_id) {
        LOG.log(Level.INFO, "User {0} is trying to unfollow lesson {1}", new Object[]{user_id, lesson_id});
        try {
            utx.begin();
            UserEntity student = em.find(UserEntity.class, user_id);
            LessonEntity lesson = em.find(LessonEntity.class, lesson_id);

            FollowEntity follow = em.find(FollowEntity.class, new FollowId(user_id, lesson_id));

            student.removeFollowedLesson(follow);
            em.merge(student);
            lesson.removeStudent(follow);
            em.merge(lesson);

            em.remove(follow);

            ctx.unfollow(user_id, lesson_id);
            utx.commit();
        } catch (IllegalStateException | SecurityException | JsonbException | HeuristicMixedException | HeuristicRollbackException | NotSupportedException | RollbackException | SystemException ex) {
            try {
                utx.rollback();
            } catch (IllegalStateException | SecurityException | SystemException ex1) {
                LOG.log(Level.SEVERE, null, ex1);
            }
            throw new WebApplicationException(ex.getMessage());
        }
    }

    @PUT
    @Path("answer_question")
    @Override
    public void answer_question(@FormParam("lesson_id") long lesson_id, @FormParam("question_id") int question_id, @FormParam("answer_id") int answer_id) {
        try {
            utx.begin();
            ctx.answerQuestion(lesson_id, question_id, answer_id);
            utx.commit();
        } catch (NotSupportedException | SystemException | RollbackException | HeuristicMixedException | HeuristicRollbackException | SecurityException | IllegalStateException ex) {
            try {
                utx.rollback();
            } catch (IllegalStateException | SecurityException | SystemException ex1) {
                LOG.log(Level.SEVERE, null, ex1);
            }
            throw new WebApplicationException(ex.getMessage());
        }
    }

    @PUT
    @Path("solve")
    @Override
    public void solve(@FormParam("id") long id) {
        LOG.log(Level.INFO, "solving lesson {0}", id);
        try {
            utx.begin();
            ctx.solveLesson(id);
            utx.commit();
        } catch (NotSupportedException | SystemException | RollbackException | HeuristicMixedException | HeuristicRollbackException | SecurityException | IllegalStateException ex) {
            try {
                utx.rollback();
            } catch (IllegalStateException | SecurityException | SystemException ex1) {
                LOG.log(Level.SEVERE, null, ex1);
            }
            throw new WebApplicationException(ex.getMessage());
        }
    }

    @PUT
    @Path("set_time")
    @Override
    public void set_time(@FormParam("lesson_id") long lesson_id, @FormParam("token_id") int token_id, @FormParam("time") long time) {
        LOG.log(Level.INFO, "setting time of token {0} of lesson {1} to {2}", new Object[]{token_id, lesson_id, time});
        try {
            utx.begin();
            ctx.setTime(lesson_id, token_id, time);
            utx.commit();
        } catch (NotSupportedException | SystemException | RollbackException | HeuristicMixedException | HeuristicRollbackException | SecurityException | IllegalStateException ex) {
            try {
                utx.rollback();
            } catch (IllegalStateException | SecurityException | SystemException ex1) {
                LOG.log(Level.SEVERE, null, ex1);
            }
            throw new WebApplicationException(ex.getMessage());
        }
    }

    @PUT
    @Path("play")
    @Override
    public void play(@FormParam("id") long id) {
        LOG.log(Level.INFO, "starting lesson {0}", id);
        try {
            utx.begin();
            ctx.play(id);
            utx.commit();
        } catch (NotSupportedException | SystemException | RollbackException | HeuristicMixedException | HeuristicRollbackException | SecurityException | IllegalStateException ex) {
            try {
                utx.rollback();
            } catch (IllegalStateException | SecurityException | SystemException ex1) {
                LOG.log(Level.SEVERE, null, ex1);
            }
            throw new WebApplicationException(ex.getMessage());
        }
    }

    @PUT
    @Path("pause")
    @Override
    public void pause(@FormParam("id") long id) {
        LOG.log(Level.INFO, "pausing lesson {0}", id);
        try {
            utx.begin();
            ctx.pause(id);
            utx.commit();
        } catch (NotSupportedException | SystemException | RollbackException | HeuristicMixedException | HeuristicRollbackException | SecurityException | IllegalStateException ex) {
            try {
                utx.rollback();
            } catch (IllegalStateException | SecurityException | SystemException ex1) {
                LOG.log(Level.SEVERE, null, ex1);
            }
            throw new WebApplicationException(ex.getMessage());
        }
    }

    @PUT
    @Path("stop")
    @Override
    public void stop(@FormParam("id") long id) {
        LOG.log(Level.INFO, "stopping lesson {0}", id);
        try {
            utx.begin();
            ctx.stop(id);
            utx.commit();
        } catch (NotSupportedException | SystemException | RollbackException | HeuristicMixedException | HeuristicRollbackException | SecurityException | IllegalStateException ex) {
            try {
                utx.rollback();
            } catch (IllegalStateException | SecurityException | SystemException ex1) {
                LOG.log(Level.SEVERE, null, ex1);
            }
            throw new WebApplicationException(ex.getMessage());
        }
    }

    @PUT
    @Path("go_to")
    @Override
    public void go_to(@FormParam("id") long id, @FormParam("time") long time) {
        LOG.log(Level.INFO, "moving lesson {0} to time {1}", new Object[]{id, time});
        try {
            utx.begin();
            ctx.go_to(id, time);
            utx.commit();
        } catch (NotSupportedException | SystemException | RollbackException | HeuristicMixedException | HeuristicRollbackException | SecurityException | IllegalStateException ex) {
            try {
                utx.rollback();
            } catch (IllegalStateException | SecurityException | SystemException ex1) {
                LOG.log(Level.SEVERE, null, ex1);
            }
            throw new WebApplicationException(ex.getMessage());
        }
    }
}
