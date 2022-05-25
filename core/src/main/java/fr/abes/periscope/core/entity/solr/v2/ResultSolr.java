package fr.abes.periscope.core.entity.solr.v2;

import fr.abes.periscope.core.entity.solr.Notice;
import lombok.Data;

import java.util.ArrayList;
import java.util.List;

@Data
public class ResultSolr {
    private List<Notice> notices;
    private List<FacetteSolr> facettes;
    private int nbPages;
    private long nbNotices;

    public ResultSolr() {
        notices = new ArrayList<>();
        facettes = new ArrayList<>();
        nbPages = 0;
        nbNotices = 0;
    }

    public void addFacette(FacetteSolr facette) {
        this.facettes.add(facette);
    }

    public void addNotice(Notice notice) { this.notices.add(notice); }

}
