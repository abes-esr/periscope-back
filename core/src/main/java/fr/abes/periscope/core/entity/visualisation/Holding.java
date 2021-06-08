package fr.abes.periscope.core.entity.visualisation;

import fr.abes.periscope.core.entity.Item;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;

import java.util.*;

@Slf4j
@Data
public class Holding extends Item {
    private String textEtatCollection;
    private String mentionDeLacune;
    private List<Sequence> sequences = new LinkedList<>();
    private List<String> erreurs = new ArrayList<>();
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
            this.addMissingSequence((SequenceEmpty) sequence);
        } else {
            this.sequences.add(sequence);
        }
    }

    private void addContinueSequence(SequenceContinue sequence) {
        Sequence seq = this.findCloser(sequence);
        int nerestIndex = this.sequences.indexOf(seq);
        this.sequences.add(nerestIndex + 1, sequence);
    }

    private void addLacuneSequence(final SequenceLacune sequenceLacune) {
        Sequence closerSequence = this.findCloser(sequenceLacune);
        if (closerSequence == null) {
            this.addErreur("Impossible d'ajouter la lacune " + sequenceLacune.toString() + " dans un état de collection vide");
        } else {
            int nearestIndex = this.sequences.indexOf(closerSequence);

            log.debug("current=" + sequenceLacune.getStartDate().getTime() + " plus proche =" + closerSequence.getStartDate().getTime() + " index =" + nearestIndex);
            if (sequenceLacune.getStartDate().after(closerSequence.getStartDate())) {
                intersperceSequence(sequenceLacune, closerSequence, nearestIndex);
            } else {
                if (sequenceLacune.getStartDate().equals(closerSequence.getStartDate())) {
                    //en cas d'égalité, on ne fait qu'ajouter la séquence lacune juste avant la séquence la plus proche
                    closerSequence.setStartDate(new GregorianCalendar(sequenceLacune.getEndDate().get(Calendar.YEAR),sequenceLacune.getEndDate().get(Calendar.MONTH), sequenceLacune.getEndDate().get(Calendar.DAY_OF_MONTH)));
                    closerSequence.getStartDate().add(Calendar.DAY_OF_MONTH, 1);
                    this.sequences.add(nearestIndex, sequenceLacune);
                } else {
                    SequenceError sequenceError = new SequenceError(sequenceLacune);
                    this.addSequence(sequenceError);
                    this.addErreur("Lacune : " + sequenceLacune.toString() + " en dehors de l'état de collection");
                }
            }
        }
    }

    private void intersperceSequence(Sequence sequenceAIntercaler, Sequence closerSequence, int nearestIndex) {
        //on copie la séquence trouvée, et on lui change sa date de début à la date de fin de la séquence de lacune -1 jour
        Sequence copySequence = (Sequence) closerSequence.clone();
        copySequence.setStartDate(new GregorianCalendar(sequenceAIntercaler.getEndDate().get(Calendar.YEAR), sequenceAIntercaler.getEndDate().get(Calendar.MONTH), sequenceAIntercaler.getEndDate().get(Calendar.DAY_OF_MONTH)));
        copySequence.getStartDate().add(Calendar.DAY_OF_MONTH, 1);
        //on change la date de fin de la séquence la plus proche à la date de début de la séquence de lacune +1 jour
        closerSequence.setEndDate(new GregorianCalendar(sequenceAIntercaler.getStartDate().get(Calendar.YEAR), sequenceAIntercaler.getStartDate().get(Calendar.MONTH), sequenceAIntercaler.getStartDate().get(Calendar.DAY_OF_MONTH)));
        closerSequence.getEndDate().add(Calendar.DAY_OF_MONTH, -1);
        //on ajoute à la liste la séquence copiée et la séquence de lacune
        this.sequences.add(nearestIndex + 1, sequenceAIntercaler);
        this.addSequence(copySequence);
    }

    private void addMissingSequence(SequenceEmpty sequence) {
        Sequence seq = this.findCloser(sequence);
        int nerestIndex = this.sequences.indexOf(seq);
        this.sequences.add(nerestIndex + 1, sequence);
    }

    public void addErreur(String message) {
        erreurs.add(message);
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

    public List<Sequence> constructContinueSequence() {
        List<Sequence> fullSequenceList = new LinkedList<>();
        if (this.sequences.size() != 0) {
            Calendar dateMinEtatCollection = this.sequences.get(0).getStartDate();
            Calendar dateMaxEtatCollection = this.sequences.get(this.sequences.size()-1).getEndDate();

            //on crée une séquence vide qui couvre l'intégralité de la période de l'état de collection
            Sequence sequenceVide = new SequenceEmpty();
            sequenceVide.setStartDate(dateMinEtatCollection);
            sequenceVide.setEndDate(dateMaxEtatCollection);

            //on parcours toutes les séquences et on les intercale dans la séquence vide précédemment créée
            this.sequences.forEach(s -> {
                Sequence closerSequence = findCloser(s);
                if (closerSequence != null) {
                    int nearestIndex = this.sequences.indexOf(closerSequence);
                    if (s.getStartDate().after(closerSequence.getStartDate())) {
                        intersperceSequence(s, closerSequence, nearestIndex);
                    } else {
                        if (s.getStartDate().equals(closerSequence.getStartDate())) {
                            //en cas d'égalité, on ne fait qu'ajouter la séquence lacune juste avant la séquence la plus proche
                            closerSequence.setStartDate(new GregorianCalendar(s.getEndDate().get(Calendar.YEAR),s.getEndDate().get(Calendar.MONTH), s.getEndDate().get(Calendar.DAY_OF_MONTH)));
                            closerSequence.getStartDate().add(Calendar.DAY_OF_MONTH, 1);
                            fullSequenceList.add(nearestIndex, s);
                        } else {
                            SequenceError sequenceError = new SequenceError(s);
                            this.addSequence(sequenceError);
                        }
                    }
                }
            });
        }
        return fullSequenceList;
    }
}
