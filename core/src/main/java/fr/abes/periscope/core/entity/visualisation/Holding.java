package fr.abes.periscope.core.entity.visualisation;

import fr.abes.periscope.core.entity.Item;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;

import java.util.*;

import static java.util.Calendar.*;

@Slf4j
@Data
public class Holding extends Item {
    private String textEtatCollection;
    private String mentionDeLacune;
    private List<Sequence> sequences = new LinkedList<>();
    private List<SequenceError> erreurs = new LinkedList<>();
    private String textLacune;
    private String note;

    public Holding(String epn) {
        this.epn = epn;
    }

    public Holding() {
        this.sequences = new ArrayList<>();
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == null) {
            return false;
        }

        if (this == obj) {
            return true;
        }

        if (getClass() != obj.getClass()) {
            return false;
        }

        return this.epn != null && epn.equals(((Holding) obj).epn);
    }

    public void addSequence(Sequence sequence) {
        if (sequence instanceof SequenceContinue) {
            this.addContinueSequence((SequenceContinue) sequence);
        } else if (sequence instanceof SequenceLacune) {
            this.addLacuneSequence((SequenceLacune) sequence);
        } else if (sequence instanceof SequenceEmpty) {
            this.addEmptySequence((SequenceEmpty) sequence);
        } else {
            this.sequences.add(sequence);
        }
    }

    private void addContinueSequence(SequenceContinue sequence) {
        Sequence closerSequence = this.findCloser(sequence);
        if (closerSequence != null) {
            int nearestIndex = this.sequences.indexOf(closerSequence);
            this.sequences.add(nearestIndex + 1, sequence);
            if (sequence.getStartDate().after(closerSequence.getEndDate()) && sequence.getStartDate().compareTo(closerSequence.getEndDate()) != 1) {
                //si la séquence à insérer est après la séquence la plus proche et qu'il y a un écart entre les deux on rajoute une séquence vide pour assurer la continuité de l'état de collection
                Sequence emptySequence = new SequenceEmpty();
                emptySequence.setStartDate(new GregorianCalendar(closerSequence.getEndDate().get(YEAR), closerSequence.getEndDate().get(MONTH), closerSequence.getEndDate().get(DAY_OF_MONTH)));
                emptySequence.getStartDate().add(DAY_OF_MONTH, -1);
                emptySequence.setEndDate(new GregorianCalendar(sequence.getStartDate().get(YEAR), sequence.getStartDate().get(MONTH), sequence.getStartDate().get(DAY_OF_MONTH)));
                emptySequence.getEndDate().add(DAY_OF_MONTH, 1);
                this.addSequence(emptySequence);
            } else if (sequence.getEndDate().before(closerSequence.getStartDate()) && sequence.getEndDate().compareTo(closerSequence.getStartDate()) != 1) {
                //si la séquence à insérer est avant la séquence la plus proche et qu'il y a un écart entre les deux on rajoute une séquence vide
                Sequence emptySequence = new SequenceEmpty();
                emptySequence.setStartDate(new GregorianCalendar(sequence.getEndDate().get(YEAR), sequence.getEndDate().get(MONTH), sequence.getEndDate().get(DAY_OF_MONTH)));
                emptySequence.getStartDate().add(DAY_OF_MONTH, 1);
                emptySequence.setEndDate(new GregorianCalendar(closerSequence.getStartDate().get(YEAR), closerSequence.getStartDate().get(MONTH), closerSequence.getStartDate().get(DAY_OF_MONTH)));
                emptySequence.getEndDate().add(DAY_OF_MONTH, -1);
                this.addSequence(emptySequence);
            }
        }
        else {
            this.sequences.add(sequence);
        }
    }

    private void addLacuneSequence(final SequenceLacune sequenceLacune) {
        Sequence closerSequence = this.findCloser(sequenceLacune);
        if (closerSequence == null) {
            SequenceError sequenceError = new SequenceError(sequenceLacune, "Impossible d'ajouter la lacune " + sequenceLacune.toString() + " dans un état de collection vide");
            this.addErreur(sequenceError);
        } else {
            int nearestIndex = this.sequences.indexOf(closerSequence);

            //log.debug("current=" + sequenceLacune.getStartDate().getTime() + " plus proche =" + closerSequence.getStartDate().getTime() + " index =" + nearestIndex);
            if (sequenceLacune.getStartDate().after(closerSequence.getStartDate())) {
                intersperceSequence(sequenceLacune, closerSequence, nearestIndex);
            } else {
                if (sequenceLacune.getStartDate().equals(closerSequence.getStartDate())) {
                    //en cas d'égalité, on ne fait qu'ajouter la séquence lacune juste avant la séquence la plus proche
                    closerSequence.setStartDate(new GregorianCalendar(sequenceLacune.getEndDate().get(YEAR),sequenceLacune.getEndDate().get(MONTH), sequenceLacune.getEndDate().get(DAY_OF_MONTH)));
                    closerSequence.getStartDate().add(DAY_OF_MONTH, 1);
                    this.sequences.add(nearestIndex, sequenceLacune);
                } else {
                    SequenceError sequenceError = new SequenceError(sequenceLacune, "Lacune : " + sequenceLacune.toString() + " en dehors de l'état de collection");
                    this.addErreur(sequenceError);
                }
            }
        }
    }

    private void intersperceSequence(Sequence sequenceAIntercaler, Sequence closerSequence, int nearestIndex) {
        //on copie la séquence trouvée, et on lui change sa date de début à la date de fin de la séquence de lacune -1 jour
        Sequence copySequence = (Sequence) closerSequence.clone();
        copySequence.setStartDate(new GregorianCalendar(sequenceAIntercaler.getEndDate().get(YEAR), sequenceAIntercaler.getEndDate().get(MONTH), sequenceAIntercaler.getEndDate().get(DAY_OF_MONTH)));
        copySequence.getStartDate().add(DAY_OF_MONTH, 1);
        //on change la date de fin de la séquence la plus proche à la date de début de la séquence de lacune +1 jour
        closerSequence.setEndDate(new GregorianCalendar(sequenceAIntercaler.getStartDate().get(YEAR), sequenceAIntercaler.getStartDate().get(MONTH), sequenceAIntercaler.getStartDate().get(DAY_OF_MONTH)));
        closerSequence.getEndDate().add(DAY_OF_MONTH, -1);
        //on ajoute à la liste la séquence copiée et la séquence de lacune
        this.sequences.add(nearestIndex + 1, sequenceAIntercaler);
        this.addSequence(copySequence);
    }

    private void addEmptySequence(SequenceEmpty sequence) {
        Sequence seq = this.findCloser(sequence);
        int nerestIndex = this.sequences.indexOf(seq);
        this.sequences.add(nerestIndex + 1, sequence);
    }

    public void addErreur(SequenceError sequenceError) {
        erreurs.add(sequenceError);
    }

    Sequence findCloser(Sequence sequence) {
        if (this.sequences.size() == 0) {
            return null;
        } else {
            return Collections.min(this.sequences, (d1, d2) -> {
                long diff1 = Math.abs(d1.getStartDate().getTime().getTime() - sequence.getStartDate().getTime().getTime());
                long diff2 = Math.abs(d2.getStartDate().getTime().getTime() - sequence.getStartDate().getTime().getTime());
                return Long.compare(diff1, diff2);
            });
        }
    }

}
