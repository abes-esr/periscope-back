package fr.abes.periscope.core.entity.visualisation;

import lombok.Data;

import java.util.LinkedList;
import java.util.List;

@Data
public class Holding {
    private String id;
    private String rcr;
    private String textEtatCollection;
    private List<Sequence> sequences;
    private Lacune lacune;
    private List<String> erreurs;

    public Holding(String id) {
        this.id = id;
        this.sequences = new LinkedList<>();
    }

    public void addSequence(Sequence sequence) {
        this.sequences.add(sequence);
    }

    public void addErreur(String message) {
        erreurs.add(message);
    }

}
