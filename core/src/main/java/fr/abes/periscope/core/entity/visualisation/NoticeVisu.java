package fr.abes.periscope.core.entity.visualisation;

import fr.abes.periscope.core.entity.Notice;
import fr.abes.periscope.core.entity.PublicationYear;
import lombok.Getter;
import lombok.Setter;

import java.util.ArrayList;
import java.util.List;

@Getter
@Setter
public class NoticeVisu extends Notice {

    private String editor;

    private String keyShortedTitle;

    private String properTitle;

    private String titleFromDifferentAuthor;

    private String parallelTitle;

    private String titleComplement;

    private String sectionTitle;

    private String keyTitleQualifer;

    private String language;

    private String country;

    private String keyTitle;

    private String frequency;

    private String continuousType;

    private String supportType;

    protected List<Holding> holdings = new ArrayList<>();

    public void addHolding(Holding holding) {
        this.holdings.add(holding);
    }


}
