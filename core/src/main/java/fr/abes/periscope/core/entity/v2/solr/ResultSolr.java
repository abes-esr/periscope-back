package fr.abes.periscope.core.entity.v2.solr;

import fr.abes.periscope.core.entity.Notice;
import fr.abes.periscope.core.entity.v2.NoticeV2;
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


}
