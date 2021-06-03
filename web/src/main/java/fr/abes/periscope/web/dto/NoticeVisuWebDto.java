package fr.abes.periscope.web.dto;

import com.fasterxml.jackson.annotation.JsonGetter;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import fr.abes.periscope.core.entity.PublicationYear;
import lombok.Getter;
import lombok.Setter;

import java.util.List;

@Getter
@Setter
public class NoticeVisuWebDto {
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

    @JsonProperty("typeSupport")
    private String typeSupport;

    @JsonProperty("dateDebutPublication")
    private PublicationYear startYear;
    @JsonProperty("dateFinPublication")
    private PublicationYear endYear;

    @JsonProperty("holdings")
    private List<HoldingWebDto> holdings;

}
