package it.cnr.istc.pst.exploraa.db;

import javax.persistence.Entity;

@Entity
public class TextRuleEntity extends RuleEntity {

    private String text;

    public String getText() {
        return text;
    }

    public void setText(final String text) {
        this.text = text;
    }
}
