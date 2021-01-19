package fr.abes.periscope.core.entity;

import lombok.Getter;
import lombok.Setter;

import java.util.Date;
import java.util.HashSet;

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

    private Date startDate;

    private Date endDate;

    @Override
    public String toString() {
        return "Notice {"+ "ppn="+ ppn+", issn="+issn+", startDate="+startDate+", endDate="+endDate+"}";
    }

}
