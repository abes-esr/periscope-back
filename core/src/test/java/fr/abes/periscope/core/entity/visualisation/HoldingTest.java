package fr.abes.periscope.core.entity.visualisation;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class HoldingTest {

    @Test
    @DisplayName("test recherche séquence la plus proche quand séquence en dehors de l'intervalle")
    void findCloserTest1() {
        Holding holding = new Holding("41133793901");
        Sequence sequenceLacune1 = new SequenceLacune(1989, "", "");
        Assertions.assertNull(holding.findIntraSequence(sequenceLacune1));

        Sequence sequence1 = new SequenceContinue(1990, "", "", 1992, "", "");
        holding.getSequences().add(sequence1);
        Assertions.assertNull(holding.findIntraSequence(sequenceLacune1));
    }

    @Test
    @DisplayName("test recherche séquence 1 quand séquence 1 incluse dans séquence existante")
    void findCloserTest2() {
        Holding holding = new Holding("41133793901");
        Sequence sequence = new SequenceContinue(1995, "", "", 1998, "", "");
        holding.getSequences().add(sequence);

        Sequence sequenceLacune = new SequenceLacune(1996, "", "");
        Assertions.assertEquals(sequence, holding.findIntraSequence(sequenceLacune));
    }

    @Test
    @DisplayName("test recherche séquence 1 quand séquence 1 incluse dans séquence existante (plusieurs séquences)")
    void findCloserTest3() {
        Holding holding = new Holding("41133793901");

        Sequence sequence = new SequenceContinue(1995, "", "", 1998, "", "");
        holding.getSequences().add(sequence);
        Sequence sequence1 = new SequenceEmpty(1999, 2000);
        holding.getSequences().add(sequence1);
        Sequence sequence2 = new SequenceContinue(2001, "", "", 2005, "", "");
        holding.getSequences().add(sequence2);

        Sequence sequenceLacune = new SequenceLacune(1999, "", "");
        Assertions.assertEquals(sequence1, holding.findIntraSequence(sequenceLacune));

        Sequence sequenceLacune2 = new SequenceLacune(2001, "", "");
        Assertions.assertEquals(sequence2, holding.findIntraSequence(sequenceLacune2));
    }

    @Test
    @DisplayName("test recherche séquence dans séquence existante avec dates de début égales")
    void findCloserTest4() {
        Holding holding = new Holding("41133793901");

        Sequence sequence = new SequenceContinue(1995, "", "", 1998, "", "");
        holding.getSequences().add(sequence);

        Sequence sequenceLacune = new SequenceLacune(1995, "", "");
        Assertions.assertEquals(sequence, holding.findIntraSequence(sequenceLacune));
    }

    @Test
    @DisplayName("test ajout d'une séquence continue avant une autre sans trou")
    void addSequenceBeforeNoGap() {
        Holding holding = new Holding("41133793901");
        Sequence sequenceToAdd1 = new SequenceContinue(2000, "", "", 2001, "", "");

        Sequence sequenceToAdd2 = new SequenceContinue(1990, "", "", 1999, "", "");

        holding.addSequence(sequenceToAdd1);
        holding.addSequence(sequenceToAdd2);

        Assertions.assertEquals(2, holding.getSequences().size());
        Assertions.assertEquals(sequenceToAdd1, holding.getSequences().get(1));
        Assertions.assertEquals(sequenceToAdd2, holding.getSequences().get(0));
    }

    @Test
    @DisplayName("test ajout d'une séquence continue après une autre sans trou")
    void addSequenceAfterNoGap() {
        Holding holding = new Holding("41133793901");
        Sequence sequenceToAdd1 = new SequenceContinue(2000, "", "", 2001,  "", "");
        Sequence sequenceToAdd2 = new SequenceContinue(1990, "", "", 1999, "", "");

        holding.addSequence(sequenceToAdd2);
        holding.addSequence(sequenceToAdd1);

        Assertions.assertEquals(2, holding.getSequences().size());
        Assertions.assertEquals(sequenceToAdd2, holding.getSequences().get(0));
        Assertions.assertEquals(sequenceToAdd1, holding.getSequences().get(1));
    }

    @Test
    @DisplayName("test ajout d'une séquence continue avant avec un trou")
    void addSequenceBeforeWithGap() {
        Holding holding = new Holding("41133793901");
        Sequence sequenceToAdd1 = new SequenceContinue(2000, "", "", 2001, "", "");

        Sequence sequenceToAdd2 = new SequenceContinue(1990, "", "", 1995, "", "");

        holding.addSequence(sequenceToAdd1);
        holding.addSequence(sequenceToAdd2);

        Assertions.assertEquals(3, holding.getSequences().size());

        // La séquence 2 est la première
        Assertions.assertEquals(sequenceToAdd2, holding.getSequences().get(0));

        // La séquence vide est la deuxième avec les bonnes dates pour faire la jointure
        Assertions.assertEquals(SequenceEmpty.class, holding.getSequences().get(1).getClass());
        Assertions.assertEquals(1996, holding.getSequences().get(1).getStartDate().intValue());
        Assertions.assertEquals(1999, holding.getSequences().get(1).getEndDate().intValue());

        Assertions.assertEquals(sequenceToAdd1, holding.getSequences().get(2));
    }

    @Test
    @DisplayName("test ajout d'une séquence continue après avec un trou")
    void addSequenceAfterWithGap() {
        Holding holding = new Holding("41133793901");
        Sequence sequenceToAdd1 = new SequenceContinue(2000, "", "", 2001, "", "");

        Sequence sequenceToAdd2 = new SequenceContinue(1990, "", "", 1995, "", "");

        holding.addSequence(sequenceToAdd2);
        holding.addSequence(sequenceToAdd1);

        Assertions.assertEquals(3, holding.getSequences().size());

        // La séquence 2 est la première
        Assertions.assertEquals(sequenceToAdd2, holding.getSequences().get(0));

        // La séquence vide est la deuxième avec les bonnes dates pour faire la jointure
        Assertions.assertEquals(SequenceEmpty.class, holding.getSequences().get(1).getClass());
        Assertions.assertEquals(1996, holding.getSequences().get(1).getStartDate().intValue());
        Assertions.assertEquals(1999, holding.getSequences().get(1).getEndDate().intValue());

        Assertions.assertEquals(sequenceToAdd1, holding.getSequences().get(2));
    }

    @Test
    @DisplayName("test ajout de lacune dans état de collection vide")
    void addLacuneOnEmptyHolding() {
        Holding holding = new Holding("41133793901");
        Sequence sequenceToAdd = new SequenceLacune(1989, "", "");
        sequenceToAdd.setEndDate(1989);
        holding.addSequence(sequenceToAdd);

        Assertions.assertEquals(1, holding.getErrorSequences().size());
        Assertions.assertEquals("Impossible d'ajouter la lacune dans un état de collection vide", holding.getErrorSequences().get(0).getMessage());
    }

    @Test
    @DisplayName("test ajout de lacune mal placée")
    void addLacuneMisplaced() {
        Holding holding = new Holding("41133793901");
        Sequence sequenceToAdd = new SequenceLacune(1989, "2139", "3000");
        sequenceToAdd.setEndDate(1989);
        Sequence sequence1 = new SequenceContinue(1990, "", "", true);
        holding.addSequence(sequence1);

        holding.addSequence(sequenceToAdd);
        Assertions.assertEquals(1, holding.getErrorSequences().size());
        Assertions.assertEquals("Lacune en dehors de l'état de collection : no.3000 vol.2139 (1989)", holding.getErrorSequences().get(0).getMessage());
    }

    @Test
    @DisplayName("test ajout lacune avec découpage de séquence existante")
    void addLacuneWithCutSequence() {
        Holding holding = new Holding("41133793901");
        Sequence lacune = new SequenceLacune(1991, "", "");
        lacune.setEndDate(1991);

        Sequence sequence = new SequenceContinue(1990, "", "", 1992, "", "");
        holding.addSequence(sequence);

        holding.addSequence(lacune);
        Assertions.assertEquals(3, holding.getSequences().size());
        Assertions.assertEquals(1990, holding.getSequences().get(0).getStartDate().intValue());
        Assertions.assertEquals(1990, holding.getSequences().get(0).getEndDate().intValue());
        Assertions.assertEquals(1991, holding.getSequences().get(1).getStartDate().intValue());
        Assertions.assertEquals(1991, holding.getSequences().get(1).getEndDate().intValue());
        Assertions.assertEquals(1992, holding.getSequences().get(2).getStartDate().intValue());
        Assertions.assertEquals(1992, holding.getSequences().get(2).getEndDate().intValue());
    }

    @Test
    @DisplayName("test ajout lacune avec découpage de séquence lacunaire existante")
    void addLacuneWithCutSequenceLacune() {
        Holding holding = new Holding("41133793901");

        Sequence sequence1 = new SequenceContinue(1988, "", "", 1993, "", "");
        holding.addSequence(sequence1);

        Sequence sequenceToAdd = new SequenceLacune(1989, 1992, "", "");
        holding.addSequence(sequenceToAdd);

        Sequence sequenceToAdd2 = new SequenceLacune(1991, "", "");
        sequenceToAdd2.setEndDate(1991);

        holding.addSequence(sequenceToAdd2);

        Assertions.assertEquals(5, holding.getSequences().size());
        Assertions.assertEquals(1988, holding.getSequences().get(0).getStartDate().intValue());
        Assertions.assertEquals(1988, holding.getSequences().get(0).getEndDate().intValue());
        Assertions.assertEquals(1989, holding.getSequences().get(1).getStartDate().intValue());
        Assertions.assertEquals(1990, holding.getSequences().get(1).getEndDate().intValue());
        Assertions.assertEquals(1991, holding.getSequences().get(2).getStartDate().intValue());
        Assertions.assertEquals(1991, holding.getSequences().get(2).getEndDate().intValue());
        Assertions.assertEquals(1992, holding.getSequences().get(3).getStartDate().intValue());
        Assertions.assertEquals(1992, holding.getSequences().get(3).getEndDate().intValue());
        Assertions.assertEquals(1993, holding.getSequences().get(4).getStartDate().intValue());
        Assertions.assertEquals(1993, holding.getSequences().get(4).getEndDate().intValue());
    }

    @Test
    @DisplayName("test ajout 2 séquences continues sans date de fin et même date de début")
    void testAddSequenceContinueWithSameStartDate() {
        Holding holding = new Holding("41133793901");
        Sequence sequence1 = new SequenceContinue(1995, "1", "", false);
        Sequence sequence2 = new SequenceContinue(1995, "2", "", false);

        holding.addSequence(sequence1);
        holding.addSequence(sequence2);

        Assertions.assertEquals(1, holding.getContinueSequences().size());
        Assertions.assertEquals(Integer.valueOf(1995), holding.getContinueSequences().get(0).getStartDate());
    }

    @Test
    @DisplayName("test ajout séquences avec séquences vides + séquence sans date de fin")
    void testAddSequenceWithEmpty() {
        Holding holding = new Holding("41133793901");
        Sequence sequence1 = new SequenceContinue(1990, "", "", 1995, "", "");
        Sequence sequence2 = new SequenceContinue(1996, "", "", 2000, "","");
        Sequence sequence3 = new SequenceContinue(2003, "", "", false);

        holding.addSequence(sequence1);
        holding.addSequence(sequence2);
        holding.addSequence(sequence3);

        Assertions.assertEquals(1990, holding.getSequences().get(0).getStartDate().intValue());
        Assertions.assertEquals(1995, holding.getSequences().get(0).getEndDate().intValue());

        Assertions.assertEquals(1996, holding.getSequences().get(1).getStartDate().intValue());
        Assertions.assertEquals(2000, holding.getSequences().get(1).getEndDate().intValue());

        Assertions.assertEquals(2001, holding.getSequences().get(2).getStartDate().intValue());
        Assertions.assertEquals(2002, holding.getSequences().get(2).getEndDate().intValue());

        Assertions.assertEquals(2003, holding.getSequences().get(3).getStartDate().intValue());
        Assertions.assertEquals(2003, holding.getSequences().get(3).getEndDate().intValue());
    }

}
