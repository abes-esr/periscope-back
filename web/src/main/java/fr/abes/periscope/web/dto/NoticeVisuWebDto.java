package fr.abes.periscope.web.dto;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import fr.abes.periscope.web.util.TYPE_SEQUENCE;
import lombok.Getter;
import lombok.Setter;
import lombok.SneakyThrows;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.List;

@Getter
@Setter
public class NoticeVisuWebDto {
    @JsonIgnore
    private String dateDebut;
    @JsonIgnore
    private String dateFin;
    @JsonProperty("holdings")
    private List<HoldingWebDto> holdingWebDtoList = new ArrayList<>();

    public NoticeVisuWebDto(String dateDebut, String dateFin) {
        this.dateDebut = dateDebut;
        this.dateFin = dateFin;
    }

    public void addHolding(HoldingWebDto holding) {
        this.holdingWebDtoList.add(holding);
    }

    /**
     * Méthode permettant de créer le premier holding de la notice correspondant à l'agrégation de tous les exemplaires affichés
     */
    public void addHoldingAgregee() throws ParseException {
        DateFormat format = new SimpleDateFormat("yyyy-MM-dd");
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
                Date date1 = format.parse(o1.getDateDebut());
                Date date2 = format.parse(o2.getDateDebut());
                return date1.compareTo(date2);
            }
        });

        StringBuilder textEtatCollection = new StringBuilder();
        Iterator<SequenceWebDto> it = sequences.listIterator();
        if (sequences.size() > 0) {
            SequenceWebDto s = it.next();
            Date previousStart = format.parse(s.getDateDebut());
            Date previousEnd = format.parse(s.getDateFin());

            while (it.hasNext()) {
                s = it.next();
                Date currentStart = format.parse(s.getDateDebut());
                Date currentEnd = format.parse(s.getDateFin());
                if (currentStart.before(previousEnd) || currentStart.equals(previousEnd)) {
                    previousEnd = (currentEnd.after(previousEnd)) ? currentEnd : previousEnd;
                } else {
                    resultSequences.add(new SequenceWebDto(format.format(previousStart.getTime()), format.format(previousEnd.getTime()), TYPE_SEQUENCE.CONTINUE, "1"));
                    textEtatCollection.append("\nCollection disponible de ").append(format.format(previousStart.getTime())).append(" à ").append(format.format(previousEnd.getTime()));
                    resultSequences.add(new SequenceWebDto(format.format(previousEnd.getTime()), format.format(currentStart.getTime()), TYPE_SEQUENCE.LACUNE, "1"));
                    textEtatCollection.append("\nCollection incomplète de ").append(format.format(previousEnd.getTime())).append(" à ").append(format.format(currentStart.getTime()));
                    previousStart = currentStart;
                    previousEnd = currentEnd;
                }
            }
            resultSequences.add(new SequenceWebDto(format.format(previousStart.getTime()), format.format(previousEnd.getTime()), TYPE_SEQUENCE.CONTINUE, "1"));
            textEtatCollection.append("\nCollection disponible de ").append(format.format(previousStart.getTime())).append(" à ").append(format.format(previousEnd.getTime()));
            holding.addSequences(resultSequences);
            holding.setEtatCollectionTextuel(textEtatCollection.toString());

        }
        else {
            holding.addSequence(new SequenceWebDto(this.getDateDebut(), this.getDateFin(), TYPE_SEQUENCE.LACUNE, "1"));
            holding.setEtatCollectionTextuel("\nCollection incomplète de " + format.format(this.getDateDebut()) + " à " + format.format(this.getDateFin()));
        }
        holdingWebDtoList.add(0, holding);
    }
}
