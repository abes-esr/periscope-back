package fr.abes.periscope.web.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class SequenceWebDto {
    @JsonProperty("dateDebut")
    private String dateDebut;
    @JsonProperty("dateFin")
    private String dateFin;
    @JsonProperty("typeSequence")
    private String typeSequence;
    @JsonProperty("rcr")
    private String rcr;
}
