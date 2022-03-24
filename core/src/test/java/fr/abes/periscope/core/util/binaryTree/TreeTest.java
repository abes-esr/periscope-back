package fr.abes.periscope.core.util.binaryTree;

import fr.abes.periscope.core.entity.visualisation.Sequence;
import fr.abes.periscope.core.entity.visualisation.SequenceContinue;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.util.Calendar;

public class TreeTest {
    @Test
    @DisplayName("test ajout de séquences successifs dans un arbre vide")
    void testAddSequenceToTree() {
        Tree tree = new Tree();

        Sequence sequence1 = new SequenceContinue(2015, Calendar.JANUARY, 1, "1", "1", true);
        tree.add(sequence1);
        Assertions.assertEquals(tree.root.getElement(), sequence1);

        Sequence sequenceDroit = new SequenceContinue(2019, Calendar.JANUARY, 1, "1", "1", true);
        tree.add(sequenceDroit);
        Assertions.assertEquals(tree.root.getDroit().getElement(), sequenceDroit);

        Sequence sequenceGauche = new SequenceContinue(2010, Calendar.JANUARY, 1, "1", "1", true);
        tree.add(sequenceGauche);
        Assertions.assertEquals(tree.root.getGauche().getElement(), sequenceGauche);

        Sequence sequenceDroitPuisGauche = new SequenceContinue(2018, Calendar.JANUARY, 1, "1", "1", true);
        tree.add(sequenceDroitPuisGauche);
        Assertions.assertEquals(tree.root.getDroit().getGauche().getElement(), sequenceDroitPuisGauche);

        Sequence sequenceGauchePuisDroit = new SequenceContinue(2012, Calendar.JANUARY, 1, "1", "1", true);
        tree.add(sequenceGauchePuisDroit);
        Assertions.assertEquals(tree.root.getGauche().getDroit().getElement(), sequenceGauchePuisDroit);

        Sequence sequenceGauchePuisGauche = new SequenceContinue(2008, Calendar.JANUARY, 1, "1", "1", true);
        tree.add(sequenceGauchePuisGauche);
        Assertions.assertEquals(tree.root.getGauche().getGauche().getElement(), sequenceGauchePuisGauche);

        Sequence sequenceDroitPuisDroit = new SequenceContinue(2022, Calendar.JANUARY, 1, "1", "1", true);
        tree.add(sequenceDroitPuisDroit);
        Assertions.assertEquals(tree.root.getDroit().getDroit().getElement(), sequenceDroitPuisDroit);
    }

    @Test
    @DisplayName("test recherche dans arbre")
    void testSearch() {
        Tree tree = new Tree();

        Sequence sequence1 = new SequenceContinue(2015, Calendar.JANUARY, 1, "1", "1", 2016, Calendar.JANUARY, 1, "1", "1");
        tree.add(sequence1);

        Sequence sequenceDroit = new SequenceContinue(2019, Calendar.JANUARY, 1, "1", "1", 2021, Calendar.JANUARY, 1, "1", "1");
        tree.add(sequenceDroit);

        Sequence sequenceGauche = new SequenceContinue(2010, Calendar.JANUARY, 1, "1", "1", 2011, Calendar.JANUARY, 1, "1", "1");
        tree.add(sequenceGauche);

        Sequence sequenceDroitPuisGauche = new SequenceContinue(2018, Calendar.JANUARY, 1, "1", "1", 2019, Calendar.JANUARY, 1, "1", "1");
        tree.add(sequenceDroitPuisGauche);

        Sequence sequenceGauchePuisDroit = new SequenceContinue(2012, Calendar.JANUARY, 1, "1", "1", 2013, Calendar.JANUARY, 1, "1", "1");
        tree.add(sequenceGauchePuisDroit);

        Sequence sequenceGauchePuisGauche = new SequenceContinue(2008, Calendar.JANUARY, 1, "1", "1", 2009, Calendar.JANUARY, 1, "1", "1");
        tree.add(sequenceGauchePuisGauche);

        Sequence sequenceDroitPuisDroit = new SequenceContinue(2022, Calendar.JANUARY, 1, "1", "1", true);
        tree.add(sequenceDroitPuisDroit);

        Sequence sequenceToSearchFor = new SequenceContinue(2018, Calendar.JANUARY, 1, "1", "1", 2019, Calendar.JANUARY, 1, "1", "1");
        Assertions.assertEquals(tree.root.getDroit().getGauche().getElement(), tree.search(tree.root, sequenceToSearchFor).getElement());

        sequenceToSearchFor = new SequenceContinue(2019, Calendar.JANUARY, 1, "1", "1", 2020, Calendar.JANUARY, 1, "1", "1");
        Assertions.assertEquals(tree.root.getDroit().getElement(), tree.search(tree.root, sequenceToSearchFor).getElement());

        sequenceToSearchFor = new SequenceContinue(2020, Calendar.JANUARY, 1, "1", "1", 2021, Calendar.JANUARY, 1, "1", "1");
        Assertions.assertEquals(tree.root.getDroit().getElement(), tree.search(tree.root, sequenceToSearchFor).getElement());

        sequenceToSearchFor = new SequenceContinue(2022, Calendar.JANUARY, 1, "1", "1", true);
        Assertions.assertEquals(tree.root.getDroit().getDroit().getElement(), tree.search(tree.root, sequenceToSearchFor).getElement());

    }
}
