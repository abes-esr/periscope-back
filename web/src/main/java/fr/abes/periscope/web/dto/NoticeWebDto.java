package fr.abes.periscope.web.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Getter;
import lombok.Setter;

import java.util.Date;
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

    @JsonProperty("date_debut")
    private Date startDate;

    @JsonProperty("date_fin")
    private Date endDate;

}
