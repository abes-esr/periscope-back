package fr.abes.periscope.core.entity.visualisation;

import fr.abes.periscope.core.entity.PublicationYear;
import lombok.Getter;
import lombok.Setter;

import java.util.ArrayList;
import java.util.List;

@Getter
@Setter
public class NoticeVisu {

    private String ppn;

    private String issn;

    private String editor;

    private String keyShortedTitle;

    private String properTitle;

    private String titleFromDifferentAuthor;

    private String parallelTitle;

    private String titleComplement;

    private String sectionTitle;

    private String keyTitleQualifer;

    private String keyTitle;

    private String frequency;

    private String continuousType;

    private String supportType;

    private PublicationYear startYear;

    private PublicationYear endYear;

    private List<Holding> holdings = new ArrayList<>();

    public void addHolding(Holding holding) {
        this.holdings.add(holding);
    }


}
