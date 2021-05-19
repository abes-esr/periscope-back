package fr.abes.periscope.core.entity.v2.solr;

import fr.abes.periscope.core.entity.Notice;
import lombok.Data;

import java.util.ArrayList;
import java.util.List;

@Data
public class ResultSolr {
    private List<Notice> notices;
    private List<FacetteSolr> facettes;
    private int nbPages;

    public ResultSolr() {
        notices = new ArrayList<>();
        facettes = new ArrayList<>();
        nbPages = 0;
    }

    public void addFacette(FacetteSolr facette) {
        this.facettes.add(facette);
    }


}