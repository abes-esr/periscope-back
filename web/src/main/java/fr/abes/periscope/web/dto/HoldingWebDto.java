package fr.abes.periscope.web.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Getter;
import lombok.Setter;

import java.util.ArrayList;
import java.util.List;

@Getter
@Setter
public class HoldingWebDto {
    @JsonProperty("erreurs")
    List<String> erreurs = new ArrayList<>();

    @JsonProperty("sequences")
    List<SequenceWebDto> sequencesList = new ArrayList<>();

    @JsonProperty("etatCollection")
    private String etatCollectionTextuel;

    public void addSequence(SequenceWebDto sequenceWebDto) {
        this.sequencesList.add(sequenceWebDto);
    }

    public void addErreurs(List<String> erreurs) {
        this.erreurs.addAll(erreurs);
    }

    public void addErreur(String erreur) { this.erreurs.add(erreur); }

    public void clearSequence() { this.sequencesList.clear(); }

    public void addSequences(List<SequenceWebDto> sequencesList) {this.sequencesList.addAll(sequencesList); }

}
