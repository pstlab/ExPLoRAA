package it.cnr.istc.exploraa;

public interface ContextListener {

    void teacherAdded(TeacherContext ctx);

    void teacherRemoved(TeacherContext t);

    void teachersCleared();

    void followingLessonAdded(FollowingLessonContext ctx);

    void followingLessonRemoved(FollowingLessonContext l);

    void followingLessonsCleared();

    void teachingLessonAdded(TeachingLessonContext ctx);

    void teachingLessonRemoved(TeachingLessonContext ctx);

    void teachingLessonsCleared();

    void studentAdded(StudentContext ctx);

    void studentRemoved(StudentContext ctx);

    void studentsCleared();
}
