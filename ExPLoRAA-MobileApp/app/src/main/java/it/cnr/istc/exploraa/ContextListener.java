package it.cnr.istc.exploraa;

public interface ContextListener {

    void teacherAdded(TeacherContext ctx);

    void followingLessonAdded(FollowingLessonContext ctx);

    void teachingLessonAdded(TeachingLessonContext ctx);

    void studentAdded(StudentContext ctx);
}
