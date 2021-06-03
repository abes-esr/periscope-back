package fr.abes.periscope.core.entity.visualisation;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;

import java.util.*;

@Slf4j
@Data
public class Holding {
    private final String COLOR_TXT_LACUNE = "";
    final long now = System.currentTimeMillis();
    private String id;
    private String rcr;
    private String textEtatCollection;
    private List<Sequence> sequences = new LinkedList<>();
    private List<String> erreurs = new ArrayList<>();
    private String commentaire;

    public Holding(String id) {
        this.id = id;
    }

    public Holding() {
        this.sequences = new ArrayList<>();
    }

    public void addSequence(Sequence sequence) {
        if (sequence instanceof SequenceContinue) {
            this.addContinueSequence((SequenceContinue) sequence);
        } else if (sequence instanceof SequenceLacune) {
            this.addLacuneSequence((SequenceLacune) sequence);
        } else if (sequence instanceof SequenceMissing) {
            this.addMissingSequence((SequenceMissing) sequence);
        } else {
            this.sequences.add(sequence);
        }
    }

    private void addContinueSequence(SequenceContinue sequence) {
        Sequence seq = this.findCloser(sequence);
        int nerestIndex = this.sequences.indexOf(seq);
        this.sequences.add(nerestIndex + 1, sequence);
    }

    private void addLacuneSequence(SequenceLacune sequence) {
        Sequence closerSequence = this.findCloser(sequence);
        if (closerSequence == null) {
            this.sequences.add(sequence);
        } else {
            int nerestIndex = this.sequences.indexOf(closerSequence);

            //TODO Copier la séquence la plus proche
            // Modifier la date de début est de fin
            // Insérer la copie de la séquence
            log.debug("current=" + sequence.getStartDate().getTime() + " plus proche =" + closerSequence.getStartDate().getTime() + " index =" + nerestIndex);
            if (sequence.getStartDate().after(closerSequence.getStartDate())) {
                this.sequences.add(nerestIndex, sequence);
            } else {
                this.sequences.add(nerestIndex + 1, sequence);
            }
        }
    }

    private void addMissingSequence(SequenceMissing sequence) {
        Sequence seq = this.findCloser(sequence);
        int nerestIndex = this.sequences.indexOf(seq);
        this.sequences.add(nerestIndex + 1, sequence);
    }

    public void addErreur(String message) {
        erreurs.add(message);
    }

    private Sequence findCloser(Sequence sequence) {
        if (this.sequences.size() == 0) {
            return null;
        } else {
            return Collections.min(this.sequences, new Comparator<Sequence>() {
                public int compare(Sequence d1, Sequence d2) {
                    long diff1 = Math.abs(d1.getStartDate().getTime().getTime() - sequence.getStartDate().getTime().getTime());
                    long diff2 = Math.abs(d2.getStartDate().getTime().getTime() - sequence.getStartDate().getTime().getTime());
                    return Long.compare(diff1, diff2);

                }
            });
        }
    }
}
