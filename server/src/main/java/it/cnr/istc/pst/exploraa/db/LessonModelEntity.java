package it.cnr.istc.pst.exploraa.db;

import java.util.HashSet;
import java.util.Set;

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.OneToMany;

@Entity
public class LessonModelEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private Long id;
    @OneToMany(cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<StimulusEntity> stimuli = new HashSet<>();

    public Long getId() {
        return id;
    }

    public Set<StimulusEntity> getStimuli() {
        return stimuli;
    }

    public void addStimulus(StimulusEntity stimulus) {
        stimuli.add(stimulus);
    }

    public void removeStimulus(StimulusEntity stimulus) {
        stimuli.remove(stimulus);
    }
}