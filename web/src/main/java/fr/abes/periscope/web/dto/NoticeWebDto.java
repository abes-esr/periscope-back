package fr.abes.periscope.web.dto;

import com.fasterxml.jackson.annotation.JsonGetter;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import fr.abes.periscope.core.entity.PublicationYear;
import lombok.Getter;
import lombok.Setter;

import java.util.HashSet;

@Getter @Setter
public class NoticeWebDto {

    @JsonProperty("ppn")
    private String ppn;

    @JsonProperty("issn")
    private String issn;

    @JsonProperty("pcpList")
    private HashSet<String> pcpList;

    @JsonProperty("rcrList")
    private HashSet<String> rcrList;

    @JsonProperty("editeur")
    private String editor;

    @JsonProperty("titre_cle")
    private String keyTitle;

    @JsonProperty("titre_court")
    private String keyShortedTitle;

    @JsonProperty("titre_propre")
    private String properTitle;

    @JsonProperty("titre_different_auteur")
    private String titleFromDifferentAuthor;

    @JsonProperty("titre_parallel")
    private String parallelTitle;

    @JsonProperty("complement_titre")
    private String titleComplement;

    @JsonProperty("section_titre")
    private String sectionTitle;

    @JsonProperty("qualitificatif")
    private String keyTitleQualifer;

    @JsonProperty("type")
    private String continiousType;

    @JsonIgnore
    private PublicationYear startYear;
    @JsonIgnore
    private PublicationYear endYear;

    @JsonGetter("date_debut")
    protected Integer getStartYear() {
        return startYear.getYear();
    }

    @JsonGetter("date_debut_indice")
    protected Integer getStartYearConfidenceIndex() {
        return startYear.getConfidenceIndex();
    }

    @JsonGetter("date_fin")
    protected Integer getEndYear() {
        return endYear.getYear();
    }

    @JsonGetter("date_fin_indice")
    protected Integer getEndYearConfidenceIndex() {
        return endYear.getConfidenceIndex();
    }

    @JsonProperty("lien_mirabel")
    private String mirabelURL;

    @JsonProperty("nb_location")
    private Integer nbLocation;

}
