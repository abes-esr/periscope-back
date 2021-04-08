package fr.abes.periscope.web.dto;

import com.fasterxml.jackson.annotation.JsonGetter;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import fr.abes.periscope.core.entity.PublicationYear;
import lombok.Getter;
import lombok.Setter;

import java.util.HashSet;
import java.util.List;

/**
 * Représente un Notice V2 au format JSON de l'API
 */
@Getter @Setter
public class NoticeWebV2Dto {

    @JsonProperty("ppn")
    private String ppn;

    @JsonProperty("issn")
    private String issn;

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
    protected Integer getEndYear() {
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

    @JsonProperty("lien_mirabel")
    private String mirabelURL;

    @JsonProperty("nb_location")
    private Integer nbLocation;

    @JsonProperty("exemplaires")
    private List<ItemWebDto> items;

}
