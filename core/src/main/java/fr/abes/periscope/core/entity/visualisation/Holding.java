package fr.abes.periscope.core.entity.visualisation;

import fr.abes.periscope.core.entity.Item;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

import java.time.Period;
import java.util.*;
import java.util.stream.Collectors;

@Slf4j
@Getter
public class Holding extends Item {
    private String textEtatCollection;
    private String mentionDeLacune;
    private List<Sequence> sequences = new LinkedList<>();
    private String textLacune;
    private String note;

    public Holding(String epn) {
        super(epn);
    }

    public void setTextEtatCollection(String value) {
        this.textEtatCollection = value;
    }

    public void setMentionDeLacune(String value) {
        this.mentionDeLacune = value;
    }

    public void setTextLacune(String value) {
        this.textLacune = value;
    }

    public void setNote(String value) {
        this.note = value;
    }

    public List<SequenceContinue> getContinueSequences() {
        return this.sequences.stream()
                .filter(p -> p instanceof SequenceContinue)
                .map(p -> (SequenceContinue) p)
                .collect(Collectors.toList());
    }

    public List<SequenceLacune> getLacuneSequences() {
        return this.sequences.stream()
                .filter(p -> p instanceof SequenceLacune)
                .map(p -> (SequenceLacune) p)
                .collect(Collectors.toList());
    }

    public List<SequenceError> getErrorSequences() {
        return this.sequences.stream()
                .filter(p -> p instanceof SequenceError)
                .map(p -> (SequenceError) p)
                .collect(Collectors.toList());
    }

    public List<SequenceEmpty> getEmptySequences() {
        return this.sequences.stream()
                .filter(p -> p instanceof SequenceEmpty)
                .map(p -> (SequenceEmpty) p)
                .collect(Collectors.toList());
    }

    public void addSequence(Sequence sequence) {
        if (sequence instanceof SequenceLacune) {
            this.addLacuneSequence((SequenceLacune) sequence);
        } else {
            Sequence nearestSeq = this.findNearestSequence(sequence);

            if (nearestSeq == null) {
                this.sequences.add(0, sequence);
            } else {

                int nearestIndex = this.sequences.indexOf(nearestSeq);
                if (sequence.getStartDate().after(nearestSeq.getStartDate())) {
                    if (sequence.getEndDate().before(nearestSeq.getEndDate()) || sequence.getEndDate().equals(nearestSeq.getEndDate())) {
                        insertInTheMiddleOfSequence(sequence, nearestSeq, nearestIndex);
                    } else {
                        insertAfterSequence(sequence, nearestIndex);
                    }
                } else {
                    insertBeforeSequence(sequence, nearestIndex);
                }
            }
        }
    }

    public void updateLacuneSequenceWithFrequency(Period frequency) {

        for (SequenceLacune lacune : getLacuneSequences()) {
            if (lacune.updateToFrequency(frequency)) {
                int index = this.sequences.indexOf(lacune);
                if (index != this.getSequences().size()) {
                    // Ce n'est pas le dernier de la liste, on mets à jour la date de début de la séquence suivante
                    this.getSequences().get(index + 1).setStartDate(lacune.getEndDate().get(Calendar.YEAR), lacune.getEndDate().get(Calendar.MONTH), lacune.getEndDate().get(Calendar.DAY_OF_MONTH));
                }
            }
        }
    }

    private void addLacuneSequence(final SequenceLacune sequence) {
        Sequence closerSequence = this.findNearestSequenceContinue(sequence);
        if (closerSequence == null) {
            SequenceError sequenceError = new SequenceError(sequence, "Impossible d'ajouter la lacune dans un état de collection vide");
            this.addSequence(sequenceError);
        } else {
            int nearestIndex = this.sequences.indexOf(closerSequence);

            log.debug("current=" + sequence.getStartDate().getTime() + " plus proche =" + closerSequence.getStartDate().getTime() + " index =" + nearestIndex);

            if (sequence.getStartDate().after(closerSequence.getStartDate())) {
                insertInTheMiddleOfSequence(sequence, closerSequence, nearestIndex);
            } else if (sequence.getStartDate().equals(closerSequence.getStartDate())) {
                insertAfterSequence(sequence, nearestIndex);
            } else {
                SequenceError sequenceError = new SequenceError(sequence, "Lacune en dehors de l'état de collection");
                this.addSequence(sequenceError);
            }
        }
    }

    private void insertAfterSequence(Sequence sequence, int nearestIndex) {
        Sequence nearestSequence = this.sequences.get(nearestIndex);

        if (nearestSequence.getEndDate().equals(sequence.getStartDate())) {
            this.sequences.add(Math.min(this.sequences.size(), nearestIndex + 1), sequence);
        } else {
            SequenceEmpty empty = new SequenceEmpty(nearestSequence.getEndDate().get(Calendar.YEAR), nearestSequence.getEndDate().get(Calendar.MONTH), nearestSequence.getEndDate().get(Calendar.DAY_OF_MONTH),
                    sequence.getStartDate().get(Calendar.YEAR), sequence.getStartDate().get(Calendar.MONTH), sequence.getStartDate().get(Calendar.DAY_OF_MONTH));

            if (!(sequence instanceof SequenceError)) {
                this.sequences.add(Math.min(this.sequences.size(), nearestIndex + 1), empty);
            }

            this.sequences.add(Math.max(this.sequences.size(), nearestIndex + 1), sequence);
        }
    }

    private void insertBeforeSequence(Sequence sequence, int nearestIndex) {
        Sequence nearestSequence = this.sequences.get(nearestIndex);

        if (nearestSequence.getStartDate().equals(sequence.getStartDate()) || nearestSequence.getStartDate().equals(sequence.getEndDate())) {
            this.sequences.add(Math.max(0, nearestIndex - 1), sequence);
        } else {
            SequenceEmpty empty = new SequenceEmpty(sequence.getEndDate().get(Calendar.YEAR), sequence.getEndDate().get(Calendar.MONTH), sequence.getEndDate().get(Calendar.DAY_OF_MONTH),
                    nearestSequence.getStartDate().get(Calendar.YEAR), nearestSequence.getStartDate().get(Calendar.MONTH), nearestSequence.getStartDate().get(Calendar.DAY_OF_MONTH));
            if (!(sequence instanceof SequenceError)) {
                this.sequences.add(Math.max(0, nearestIndex), empty);
            }
            this.sequences.add(Math.max(0, nearestIndex), sequence);
        }
    }

    private void insertInTheMiddleOfSequence(Sequence sequenceAIntercaler, Sequence nearestSequence, int nearestIndex) {
        //on copie la séquence trouvée, et on lui change sa date de début à la date de fin de la séquence de lacune
        Sequence sequenceAfter = (Sequence) nearestSequence.clone();

        sequenceAfter.setStartDate(sequenceAIntercaler.getEndDate().get(Calendar.YEAR), sequenceAIntercaler.getEndDate().get(Calendar.MONTH), sequenceAIntercaler.getEndDate().get(Calendar.DAY_OF_MONTH));
        //on change la date de fin de la séquence la plus proche à la date de début de la séquence de lacune
        nearestSequence.setEndDate(sequenceAIntercaler.getStartDate().get(Calendar.YEAR), sequenceAIntercaler.getStartDate().get(Calendar.MONTH), sequenceAIntercaler.getStartDate().get(Calendar.DAY_OF_MONTH));
        //on ajoute à la liste la séquence copiée et la séquence de lacune
        this.sequences.add(Math.min(this.sequences.size(), nearestIndex + 1), sequenceAIntercaler);

        if (!sequenceAfter.getStartDate().equals(sequenceAfter.getEndDate())) {
            this.sequences.add(Math.min(this.sequences.size(), nearestIndex + 2), sequenceAfter);
        }
    }

    public Sequence findNearestSequence(Sequence sequence) {
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

    public Sequence findNearestSequenceContinue(Sequence sequence) {
        if (this.sequences.stream()
                .filter(p -> p instanceof SequenceContinue)
                .map(p -> (SequenceContinue) p)
                .collect(Collectors.toList()).size() == 0) {
            return null;
        } else {
            return Collections.min(this.sequences.stream()
                    .filter(p -> p instanceof SequenceContinue)
                    .map(p -> (SequenceContinue) p)
                    .collect(Collectors.toList()), (d1, d2) -> {
                long diff1 = Math.abs(d1.getStartDate().getTime().getTime() - sequence.getStartDate().getTime().getTime());
                long diff2 = Math.abs(d2.getStartDate().getTime().getTime() - sequence.getStartDate().getTime().getTime());
                return Long.compare(diff1, diff2);
            });
        }
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
}
