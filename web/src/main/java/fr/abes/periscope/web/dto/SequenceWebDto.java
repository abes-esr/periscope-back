package fr.abes.periscope.web.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import fr.abes.periscope.web.util.TYPE_SEQUENCE;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@AllArgsConstructor
public class SequenceWebDto {
    @JsonProperty("anneeDebut")
    private int anneeDebut;
    @JsonProperty("anneeFin")
    private int anneeFin;
    @JsonProperty("typeSequence")
    private TYPE_SEQUENCE typeSequence;
    @JsonProperty("rcr")
    private String rcr;

    public SequenceWebDto(int anneeDebut, int anneeFin, String rcr) {
        this.anneeDebut = anneeDebut;
        this.anneeFin = anneeFin;
        this.rcr = rcr;
    }

    @Override
    public String toString() {
        return "Sequence " + this.typeSequence.toString() + " {" + "startDate=" + this.anneeDebut + ", endDate=" + this.anneeFin + "}";
    }

}
