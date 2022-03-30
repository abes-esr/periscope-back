package fr.abes.periscope.core.entity.visualisation;

import fr.abes.periscope.core.entity.solr.Item;
import fr.abes.periscope.core.util.binaryTree.Node;
import fr.abes.periscope.core.util.binaryTree.Tree;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

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
    private List<String> erreurs = new ArrayList<>();

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

    public void addErreur(String message) {
        this.erreurs.add(message);
    }

    public List<Sequence> getAllNonEmptySequences() {
        return this.sequences.stream()
                .filter(p -> (p instanceof SequenceLacune || p instanceof SequenceContinue || p instanceof SequenceError))
                .collect(Collectors.toList());
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

    public List<Sequence> getContinueSequenceAndLacunes() {
        return this.sequences.stream()
                .filter(s -> (s instanceof SequenceContinue || s instanceof SequenceLacune))
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

    /**
     * Permet d'ajouter une séquence à un état de collection
     *
     * @param sequence la séquence à ajouter à l'état de collection
     */
    public void addSequence(Sequence sequence) {
        if (sequence instanceof SequenceLacune) {
            this.addLacuneSequence((SequenceLacune) sequence);
        } else {
            //si la séquence n'est pas déjà présente dans l'état de collection
            if (!this.getContinueSequences().stream().anyMatch(s -> (s.getStartDate().equals(sequence.getStartDate()) && s.getEndDate().equals(sequence.getEndDate())))) {
                Sequence intraSequence = this.findIntraSequence(sequence);
                if (intraSequence != null) {
                    int nearestIndex = this.sequences.indexOf(intraSequence);
                    if (sequence.getStartDate() > intraSequence.getStartDate() && sequence.getStartDate() < intraSequence.getEndDate()) {
                        insertInTheMiddleOfSequence(sequence, intraSequence, nearestIndex);
                    } else if (sequence.getStartDate().equals(intraSequence.getStartDate()) || sequence.getStartDate() > intraSequence.getEndDate()) {
                        insertAfterSequence(sequence, nearestIndex);
                    }
                } else {
                    Sequence nearestSeq = this.findNearestSequence(sequence);

                    if (nearestSeq == null) {
                        this.sequences.add(0, sequence);
                    } else {

                        int nearestIndex = this.sequences.indexOf(nearestSeq);
                        if (sequence.getStartDate() > nearestSeq.getStartDate()) {
                            if (sequence.getEndDate() <= nearestSeq.getEndDate()) {
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
        }
    }

    private void addLacuneSequence(final SequenceLacune sequence) {
        //on ne fait le traitement que si on n'a pas déjà ajouté une lacune identique dans la liste
        if (this.getLacuneSequences().stream().filter(s -> s.getStartDate().equals(sequence.getStartDate()) && s.getEndDate().equals(sequence.getEndDate())).count() == 0) {
            Sequence closerSequence = this.findIntraSequence(sequence);
            if (this.sequences.size() == 0) {
                SequenceError sequenceError = new SequenceError(sequence, "Impossible d'ajouter la lacune dans un état de collection vide");
                this.addSequence(sequenceError);
            } else {
                if (closerSequence == null) {
                    SequenceError sequenceError = new SequenceError(sequence, "Lacune en dehors de l'état de collection : " + ((!sequence.getNumero().equals("Non renseigné")) ? "no." + sequence.getNumero() : "") + ((!sequence.getVolume().equals("Non renseigné")) ? " vol." + sequence.getVolume() : "") + " (" + sequence.getStartDate() + ")");
                    this.addSequence(sequenceError);
                } else {
                    int nearestIndex = this.sequences.indexOf(closerSequence);

                    log.debug(this.getEpn() + "current=" + sequence.getStartDate() + " plus proche =" + closerSequence.getStartDate() + " index =" + nearestIndex);
                    if (sequence.getStartDate() <= closerSequence.getStartDate())
                        insertBeforeSequence(sequence, nearestIndex);
                    else {
                        if (sequence.getStartDate() > closerSequence.getStartDate() && sequence.getStartDate() < closerSequence.getEndDate()) {
                            insertInTheMiddleOfSequence(sequence, closerSequence, nearestIndex);
                        } else if (sequence.getStartDate().equals(closerSequence.getStartDate())) {
                            insertAfterSequence(sequence, nearestIndex);
                        }
                    }
                }
            }
        }
    }

    /**
     * Méthode permettant d'insérer une séquence après un index de la liste donné
     *
     * @param sequence     séquence à insérer
     * @param nearestIndex index dans la liste des séquences où placer la séquence à insérer
     */
    private void insertAfterSequence(Sequence sequence, int nearestIndex) {
        //récupération de la séquence à l'index fourni
        Sequence nearestSequence = this.sequences.get(nearestIndex);
        //si la séquence la plus proche a une date de fin égale à la date de début de la séquence à insérer, on ajoute directement la séquence à insérer après la séquence la plus proche
        if (nearestSequence.getEndDate().equals(sequence.getStartDate() - 1)) {
            this.sequences.add(Math.min(this.sequences.size(), nearestIndex + 1), sequence);
        } else {
            if (!(sequence instanceof SequenceError)) {
                //on crée une séquence vide avec une date de début = date de fin de la séquence la plus proche + 1 et date de fin = date de début de la séquence à insérer - 1
                SequenceEmpty empty = new SequenceEmpty(nearestSequence.getEndDate() + 1, sequence.getStartDate() - 1);
                //on ajoute la séquence vide crée après la séquence la plus proche
                this.sequences.add(Math.min(this.sequences.size(), nearestIndex + 1), empty);
            }
            //on ajoute la séquence après la séquence vide
            this.sequences.add(Math.max(this.sequences.size(), nearestIndex + 1), sequence);
        }
    }

    /**
     * Méthode permettant d'insérer une séquence avant un index de la liste donné
     *
     * @param sequence     séquence à insérer
     * @param nearestIndex index dans la liste des séquences où placer la séquence à insérer
     */
    private void insertBeforeSequence(Sequence sequence, int nearestIndex) {
        //récupération de la séquence à l'index fourni
        Sequence nearestSequence = this.sequences.get(nearestIndex);

        //si la séquence la plus proche a une date de début égale à la date de début de la séquence à insérer ou une date de début égale à la date de fin de la séquence à insérer
        if (nearestSequence.getStartDate().equals(sequence.getStartDate()) || nearestSequence.getStartDate().equals(sequence.getEndDate())) {
            //on ajoute la séquence juste avant la séquence la plus proche
            this.sequences.add(Math.max(0, nearestIndex - 1), sequence);
        } else {
            //on crée une séquence vide avec une date de début = date de fin de la séquence à insérer + 1 et une date de fin = date de début de la séquence la plus proche - 1
            SequenceEmpty empty = new SequenceEmpty(sequence.getEndDate() + 1, nearestSequence.getStartDate() - 1);
            //on ajoute la séquence vide avant la séquence la plus proche
            this.sequences.add(Math.max(0, nearestIndex), empty);
            //on ajoute la séquence à insérer après la séquence vide
            this.sequences.add(Math.max(0, nearestIndex), sequence);
        }
    }

    /**
     * Permet l'insertion d'une séquence dans une séquence qui la "couvre"
     *
     * @param sequenceAIntercaler séquence à insérer
     * @param initSequence        séquence à découper
     * @param initSequenceIndex   index de la séquence à découper
     */
    private void insertInTheMiddleOfSequence(Sequence sequenceAIntercaler, Sequence initSequence, int initSequenceIndex) {
        //on copie la séquence à découper, qui sera placée à la fin
        Sequence sequenceAfter = (Sequence) initSequence.clone();

        //on change l'année de début de la date copiée à l'année de fin de la séquence à intercaler + 1
        sequenceAfter.setStartDate(sequenceAIntercaler.getEndDate() + 1);
        //on change l'année de fin de la séquence initiale à l'année de début de la séquence à insérer - 1
        initSequence.setEndDate(sequenceAIntercaler.getStartDate() - 1);
        //on ajoute à la liste la séquence copiée et la séquence à insérer
        this.sequences.add(Math.min(this.sequences.size(), initSequenceIndex + 1), sequenceAIntercaler);
        //on ajout à la fin de la liste la séquence copiée
        this.sequences.add(Math.min(this.sequences.size(), initSequenceIndex + 2), sequenceAfter);
    }

    /**
     * Méthode permettant de trouver une séquence dans un arbre binaire composé des séquences existante de l'état de collection
     *
     * @param sequence séquence à chercher
     * @return une séquence qui "couvre" la séquence à chercher
     */
    public Sequence findIntraSequence(Sequence sequence) {
        if (this.sequences.size() == 0) {
            return null;
        } else {
            Tree tree = new Tree();
            this.sequences.forEach(s -> tree.add(s));
            Node nodeFound = tree.search(tree.root, sequence);
            return (nodeFound != null) ? nodeFound.getElement() : null;
        }
    }

    /**
     * Permet de trouver la séquence la plus proche de la séquence à chercher
     *
     * @param sequence séquence à chercher dans les séquences déjà existantes
     * @return la séquence la plus proche (le calcul se fait sur les dates de début)
     */
    public Sequence findNearestSequence(Sequence sequence) {
        if (this.sequences.size() == 0) {
            return null;
        }
        return Collections.min(this.sequences, (d1, d2) -> {
            long diff1 = Math.abs(d1.getStartDate() - sequence.getStartDate());
            long diff2 = Math.abs(d2.getStartDate() - sequence.getStartDate());
            return Long.compare(diff1, diff2);
        });
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
