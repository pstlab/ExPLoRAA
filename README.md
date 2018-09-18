# ExPLoRAA (ExPeriential LeaRning for Active Aging)

Starting from a static representation containing an high-level lesson track, initially stored in the database, a *lesson* is planned and dynamically adapted and personalized for the involved users.

The idea of using the technology related to Artificial Intelligence (AI) comes from the need to create a sufficiently extensive didactic experience to reproduce a large number of different situations which are, at the same time, characterized by a high variability of stimuli, aimed at increasing the involvement level of users. Automated planning, indeed, favors the generation of different
lessons that would be too complicated to obtain with a simple pre-compilation of stories. The timeline-based approach to planning, in particular, represents the unifying element of the various modules by ensuring the dynamic adaptability of plans by promoting experiential learning.

From a high-level point of view, it is possible to distinguish between two kinds of involved users: the *students*, i.e. a group of people, potentially, of any age, interested in using the learning services offered by the ExPLoRAA, and the *teachers*, i.e. users with special privileges who have the opportunity to observe students, monitoring the progress of the actions and the learning environment. The above users interact with the LECTurE system which is composed of three functional blocks, intended as architectural subsystems, implementing the corresponding high-level functionalities: (i) the *user modeling*, whose goal is to create and maintain a user model and provide guidance for improving the learning process; (ii) the *lesson modeling*, whose role is to combine the information from the previous subsystem and to create the customized lesson as well as to control its evolution; (iii) the *lesson presentation*, whose purpose is to effectively represent the lesson.

&copy; 2018 [Planning and Scheduling Technology Laboratory](http://pst.istc.cnr.it/)
