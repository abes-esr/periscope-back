package fr.abes.periscope.web.dto;

import com.fasterxml.jackson.annotation.JsonGetter;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import fr.abes.periscope.core.entity.solr.PublicationYear;
import lombok.Getter;
import lombok.Setter;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Représente un Notice V2 au format JSON de l'API
 */
@Getter
@Setter
public class NoticeWebV2Dto {

    @JsonProperty("ppn")
    private String ppn;

    @JsonProperty("issn")
    private String issn;

    @JsonProperty("editeur")
    private String editeur;

    @JsonProperty("titre")
    private String titre;

    @JsonProperty("typeDocument")
    private String typeRessourceContinue;

    @JsonIgnore
    private PublicationYear startYear;
    @JsonIgnore
    private PublicationYear endYear;

    @JsonGetter("date_debut")
    protected String getStartYear() {
        if (startYear == null) {
            return null;
        } else {
            return startYear.getYear();
        }
    }

    @JsonGetter("date_debut_indice")
    protected Integer getStartYearConfidenceIndex() {
        if (startYear == null) {
            return null;
        } else {
            return startYear.getConfidenceIndex();
        }
    }

    @JsonGetter("date_fin")
    protected String getEndYear() {
        if (endYear == null) {
            return null;
        } else {
            return endYear.getYear();
        }
    }

    @JsonGetter("date_fin_indice")
    protected Integer getEndYearConfidenceIndex() {
        if (endYear == null) {
            return null;
        } else {
            return endYear.getConfidenceIndex();
        }
    }


    @JsonProperty("nb_location")
    private Integer nbLocation;

    @JsonProperty("lien_sudoc")
    private String sudocURL;

    @JsonProperty("pcpList")
    private Set<String> pcpList = new HashSet<>();

    @JsonProperty("rcrList")
    private List<String> rcrList = new ArrayList<>();

    public void addPcp(String pcp) {
        this.pcpList.add(pcp);
    }

    public void addRcr(String rcr) {
        this.rcrList.add(rcr);
    }


}
