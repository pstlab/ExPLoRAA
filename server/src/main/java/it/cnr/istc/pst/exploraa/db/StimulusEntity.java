package it.cnr.istc.pst.exploraa.db;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import javax.persistence.CascadeType;
import javax.persistence.ElementCollection;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Inheritance;
import javax.persistence.InheritanceType;
import javax.persistence.ManyToMany;

@Entity
@Inheritance(strategy = InheritanceType.SINGLE_TABLE)
public class StimulusEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private Long id;
    @ElementCollection
    private Set<String> topics = new HashSet<>();
    private Long length;
    @ManyToMany(mappedBy = "effects", cascade = CascadeType.ALL)
    private Set<StimulusEntity> preconditions = new HashSet<>();
    @ManyToMany(cascade = CascadeType.ALL)
    private Set<StimulusEntity> effects = new HashSet<>();

    public Long getId() {
        return id;
    }

    public Set<String> getTopics() {
        return Collections.unmodifiableSet(topics);
    }

    public void addTopic(String topic) {
        topics.add(topic);
    }

    public void removeTopic(String topic) {
        topics.remove(topic);
    }

    public Long getLength() {
        return length;
    }

    public void setLength(Long length) {
        this.length = length;
    }

    public Set<StimulusEntity> getPreconditions() {
        return Collections.unmodifiableSet(preconditions);
    }

    public void addPrecondition(StimulusEntity precondition) {
        preconditions.add(precondition);
    }

    public void removePrecondition(StimulusEntity precondition) {
        preconditions.remove(precondition);
    }

    public Set<StimulusEntity> getEffects() {
        return Collections.unmodifiableSet(effects);
    }

    public void addEffect(StimulusEntity effect) {
        effects.add(effect);
    }

    public void removeEffect(StimulusEntity effect) {
        effects.remove(effect);
    }
}
