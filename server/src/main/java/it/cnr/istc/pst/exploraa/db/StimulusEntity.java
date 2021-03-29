package it.cnr.istc.pst.exploraa.db;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

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
    private final Set<String> topics = new HashSet<>();
    private Long length;
    @ManyToMany(mappedBy = "effects")
    private final Set<StimulusEntity> preconditions = new HashSet<>();
    @ManyToMany
    private final Set<StimulusEntity> effects = new HashSet<>();

    public Long getId() {
        return id;
    }

    public Set<String> getTopics() {
        return Collections.unmodifiableSet(topics);
    }

    public void addTopic(final String topic) {
        topics.add(topic);
    }

    public void removeTopic(final String topic) {
        topics.remove(topic);
    }

    public Long getLength() {
        return length;
    }

    public void setLength(final Long length) {
        this.length = length;
    }

    public Set<StimulusEntity> getPreconditions() {
        return Collections.unmodifiableSet(preconditions);
    }

    public void addPrecondition(final StimulusEntity precondition) {
        preconditions.add(precondition);
    }

    public void removePrecondition(final StimulusEntity precondition) {
        preconditions.remove(precondition);
    }

    public Set<StimulusEntity> getEffects() {
        return Collections.unmodifiableSet(effects);
    }

    public void addEffect(final StimulusEntity effect) {
        effects.add(effect);
    }

    public void removeEffect(final StimulusEntity effect) {
        effects.remove(effect);
    }
}
