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
    private String name;
    @OneToMany(cascade = CascadeType.ALL, orphanRemoval = true)
    private final Set<StimulusEntity> stimuli = new HashSet<>();

    public Long getId() {
        return id;
    }

    public String getName() {
        return name;
    }

    public void setName(final String name) {
        this.name = name;
    }

    public Set<StimulusEntity> getStimuli() {
        return stimuli;
    }

    public void addStimulus(final StimulusEntity stimulus) {
        stimuli.add(stimulus);
    }

    public void removeStimulus(final StimulusEntity stimulus) {
        stimuli.remove(stimulus);
    }
}