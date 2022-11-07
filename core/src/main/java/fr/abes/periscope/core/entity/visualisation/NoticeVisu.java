package fr.abes.periscope.core.entity.visualisation;

import fr.abes.periscope.core.entity.solr.Notice;
import lombok.Getter;
import lombok.Setter;

import java.time.Period;
import java.util.HashSet;
import java.util.Set;

@Getter
@Setter
public class NoticeVisu extends Notice {
    private String keyShortedTitle;

    private String properTitle;

    private String titleFromDifferentAuthor;

    private String parallelTitle;

    private String titleComplement;

    private String sectionTitle;

    private String keyTitleQualifer;

    private String frequency;

    private String city;

    protected Set<Holding> holdings = new HashSet<>();

    public void addHolding(Holding holding) {
        this.holdings.add(holding);
    }



}
