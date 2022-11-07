package fr.abes.periscope.web.dto;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import fr.abes.periscope.web.util.TYPE_SEQUENCE;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.SneakyThrows;

import java.util.*;

@Getter
@Setter
@NoArgsConstructor
public class NoticeVisuWebDto {
    @JsonIgnore
    private int anneeDebut;
    @JsonIgnore
    private int anneeFin;
    @JsonProperty("holdings")
    private List<HoldingWebDto> holdingWebDtoList = new ArrayList<>();

    public NoticeVisuWebDto(int anneeDebut, int anneeFin) {
        this.anneeDebut = anneeDebut;
        this.anneeFin = anneeFin;
    }

    public void addHolding(HoldingWebDto holding) {
        this.holdingWebDtoList.add(holding);
    }

    /**
     * Méthode permettant de créer le premier holding de la notice correspondant à l'agrégation de tous les exemplaires affichés
     * les séquences contenues dans les différents holdings doivent être contigues et sans chevauchement
     */
    public void addHoldingAgregee() {
        HoldingWebDto holding = new HoldingWebDto();
        List<SequenceWebDto> sequences = new ArrayList<>();
        List<SequenceWebDto> resultSequences = new ArrayList<>();
        this.holdingWebDtoList.forEach(h -> {
            sequences.addAll(h.getSequencesList());
        });
        Collections.sort(sequences, new Comparator<SequenceWebDto>() {
            @SneakyThrows
            @Override
            public int compare(SequenceWebDto o1, SequenceWebDto o2) {
                if (o1.getAnneeDebut() == o2.getAnneeDebut()) return 0;
                if (o1.getAnneeDebut() < o2.getAnneeDebut()) return -1;
                return 1;
            }
        });

        StringBuilder textEtatCollection = new StringBuilder();
        Iterator<SequenceWebDto> it = sequences.listIterator();
        if (it.hasNext()) {
            SequenceWebDto s = it.next();
            int previousStart = s.getAnneeDebut();
            int previousEnd = s.getAnneeFin();

            while (it.hasNext()) {
                s = it.next();
                int currentStart = s.getAnneeDebut();
                int currentEnd = s.getAnneeFin();
                if (currentStart < previousEnd || currentStart == previousEnd) {
                    previousEnd = (currentEnd > previousEnd) ? currentEnd : previousEnd;
                } else {
                    resultSequences.add(new SequenceWebDto(previousStart, previousEnd, TYPE_SEQUENCE.CONTINUE, "1"));
                    textEtatCollection.append("\nCollection disponible de ").append(previousStart).append(" à ").append(previousEnd);
                    resultSequences.add(new SequenceWebDto(previousEnd, currentStart, TYPE_SEQUENCE.LACUNE, "1"));
                    textEtatCollection.append("\nCollection incomplète de ").append(previousEnd).append(" à ").append(currentStart);
                    previousStart = currentStart;
                    previousEnd = currentEnd;
                }
            }
            resultSequences.add(new SequenceWebDto(previousStart, previousEnd, TYPE_SEQUENCE.CONTINUE, "1"));
            textEtatCollection.append("\nCollection disponible de ").append(previousStart).append(" à ").append(previousEnd);
            holding.addSequences(resultSequences);
            holding.setEtatCollectionTextuel(textEtatCollection.toString());

        }
        else {
            holding.addSequence(new SequenceWebDto(this.getAnneeDebut(), this.getAnneeFin(), TYPE_SEQUENCE.LACUNE, "1"));
            holding.setEtatCollectionTextuel("\nCollection incomplète de " + this.getAnneeDebut() + " à " + this.getAnneeFin());
        }
        holdingWebDtoList.add(0, holding);
    }
}
