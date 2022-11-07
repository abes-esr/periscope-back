package fr.abes.periscope.core.entity.solr;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.HashSet;
import java.util.Set;

/**
 * Représente une Notice au format Periscope
 */
@Getter @Setter
@NoArgsConstructor
public class Notice {

    protected String ppn;

    protected String issn;

    protected String publisher;

    protected String keyTitle;

    protected String continuousType;

    protected String supportType;

    protected PublicationYear startYear;

    protected PublicationYear endYear;

    protected String mirabelURL;

    protected Integer nbLocation;

    private String keyShortedTitle;

    private String properTitle;

    private String titleFromDifferentAuthor;

    private String parallelTitle;

    private String titleComplement;

    private String sectionTitle;

    private String keyTitleQualifer;

    private String language;

    private String country;

    private Integer nbPcp;

    private Set<String> pcpList = new HashSet<>();

    private Set<Item> items = new HashSet<>();

    public void addItem(Item item) {
        items.add(item);
    }

    public void addPcp(String pcp) { this.pcpList.add(pcp); }


    @Override
    public boolean equals(Object obj) {
        if (obj == null) {
            return false;
        }

        if (this == obj) {
            return true;
        }

        if (getClass() != obj.getClass()) {
            return false;
        }

        return ppn != null && ppn.equals(((Notice) obj).ppn);
    }

    @Override
    public int hashCode() {
        return 2020;
    }

    @Override
    public String toString() {
        return "Notice {"+ "ppn="+ ppn+", issn="+issn+", startDate="+ startYear +", endDate="+ endYear +"}";
    }

}
