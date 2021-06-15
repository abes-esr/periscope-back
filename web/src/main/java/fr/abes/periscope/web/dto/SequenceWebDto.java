package fr.abes.periscope.web.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import fr.abes.periscope.web.util.TYPE_SEQUENCE;
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
    private TYPE_SEQUENCE typeSequence;
    @JsonProperty("rcr")
    private String rcr;
}
