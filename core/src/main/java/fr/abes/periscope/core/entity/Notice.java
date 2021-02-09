package fr.abes.periscope.core.entity;

import lombok.Getter;
import lombok.Setter;

import java.util.HashSet;

/**
 * Représente une Notice au format Periscope
 */
@Getter
@Setter
public class Notice {

    private String ppn;

    private String issn;

    private HashSet<String> pcpList;

    private HashSet<String> rcrList;

    private String editor;

    private String keyTitle;

    private String keyShortedTitle;

    private String properTitle;

    private String titleFromDifferentAuthor;

    private String parallelTitle;

    private String titleComplement;

    private String sectionTitle;

    private String keyTitleQualifer;

    private String continiousType;

    private PublicationYear startYear;

    private PublicationYear endYear;

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
