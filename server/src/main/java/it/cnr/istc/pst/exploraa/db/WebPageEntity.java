package it.cnr.istc.pst.exploraa.db;

import javax.persistence.Entity;

@Entity
public class WebPageEntity extends StimulusEntity {

    private String url;

    public String getUrl() {
        return url;
    }

    public void setUrl(final String url) {
        this.url = url;
    }
}