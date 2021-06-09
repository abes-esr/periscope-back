package fr.abes.periscope.core.entity.visualisation;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.util.Calendar;
import java.util.GregorianCalendar;

public class HoldingTest {

    @Test
    void findCloserTest() {
        Holding holding = new Holding("41133793901");
        Sequence sequenceToAdd = new SequenceLacune(1989, Calendar.FEBRUARY, 28, "", "");
        Assertions.assertNull(holding.findNearestSequence(sequenceToAdd));

        Sequence sequence1 = new SequenceContinue(1990, Calendar.JANUARY, 1, "", "");
        holding.addSequence(sequence1);
        Assertions.assertEquals(sequence1, holding.findNearestSequence(sequenceToAdd));

        Sequence sequence2 = new SequenceContinue(1995, Calendar.JANUARY, 31, "", "");
        holding.addSequence(sequence2);

        sequenceToAdd.setStartDate(1996, Calendar.MARCH, 31);

        Assertions.assertEquals(2, holding.getSequences().size());
        Assertions.assertEquals(sequence1, holding.findNearestSequence(sequenceToAdd));
    }

    @Test
    @DisplayName("test ajout d'une séquence continue avant une autre sans trou")
    void addSequenceBeforeNoGap() {
        Holding holding = new Holding("41133793901");
        Sequence sequenceToAdd1 = new SequenceContinue(2000, Calendar.JANUARY, 1, "", "");
        sequenceToAdd1.setEndDate(2001, Calendar.JANUARY, 1);

        Sequence sequenceToAdd2 = new SequenceContinue(1990, Calendar.JANUARY, 1, "", "");
        sequenceToAdd2.setEndDate(2000, Calendar.JANUARY, 1);

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
        Sequence sequenceToAdd1 = new SequenceContinue(2000, Calendar.JANUARY, 1, "", "");
        sequenceToAdd1.setEndDate(2001, Calendar.JANUARY, 1);

        Sequence sequenceToAdd2 = new SequenceContinue(1990, Calendar.JANUARY, 1, "", "");
        sequenceToAdd2.setEndDate(2000, Calendar.JANUARY, 1);

        holding.addSequence(sequenceToAdd2);
        holding.addSequence(sequenceToAdd1);

        Assertions.assertEquals(2, holding.getSequences().size());
        Assertions.assertEquals(sequenceToAdd1, holding.getSequences().get(1));
        Assertions.assertEquals(sequenceToAdd2, holding.getSequences().get(0));
    }

    @Test
    @DisplayName("test ajout d'une séquence continue avant avec un trou")
    void addSequenceBeforeWithGap() {
        Holding holding = new Holding("41133793901");
        Sequence sequenceToAdd1 = new SequenceContinue(2000, Calendar.JANUARY, 1, "", "");
        sequenceToAdd1.setEndDate(2001, Calendar.JANUARY, 1);

        Sequence sequenceToAdd2 = new SequenceContinue(1990, Calendar.JANUARY, 1, "", "");
        sequenceToAdd2.setEndDate(1995, Calendar.JANUARY, 1);

        holding.addSequence(sequenceToAdd1);
        holding.addSequence(sequenceToAdd2);

        Assertions.assertEquals(3, holding.getSequences().size());

        // La séquence 2 est la première
        Assertions.assertEquals(sequenceToAdd2, holding.getSequences().get(0));

        // La séquence vide est la deuxième avec les bonnes dates pour faire la jointure
        Assertions.assertEquals(SequenceEmpty.class, holding.getSequences().get(1).getClass());
        Assertions.assertEquals(new GregorianCalendar(1995, Calendar.JANUARY, 1), holding.getSequences().get(1).getStartDate());
        Assertions.assertEquals(new GregorianCalendar(2000, Calendar.JANUARY, 1), holding.getSequences().get(1).getEndDate());

        Assertions.assertEquals(sequenceToAdd1, holding.getSequences().get(2));
    }

    @Test
    @DisplayName("test ajout d'une séquence continue après avec un trou")
    void addSequenceAfterWithGap() {
        Holding holding = new Holding("41133793901");
        Sequence sequenceToAdd1 = new SequenceContinue(2000, Calendar.JANUARY, 1, "", "");
        sequenceToAdd1.setEndDate(2001, Calendar.JANUARY, 1);

        Sequence sequenceToAdd2 = new SequenceContinue(1990, Calendar.JANUARY, 1, "", "");
        sequenceToAdd2.setEndDate(1995, Calendar.JANUARY, 1);

        holding.addSequence(sequenceToAdd2);
        holding.addSequence(sequenceToAdd1);

        Assertions.assertEquals(3, holding.getSequences().size());

        // La séquence 2 est la première
        Assertions.assertEquals(sequenceToAdd2, holding.getSequences().get(0));

        // La séquence vide est la deuxième avec les bonnes dates pour faire la jointure
        Assertions.assertEquals(SequenceEmpty.class, holding.getSequences().get(1).getClass());
        Assertions.assertEquals(new GregorianCalendar(1995, Calendar.JANUARY, 1), holding.getSequences().get(1).getStartDate());
        Assertions.assertEquals(new GregorianCalendar(2000, Calendar.JANUARY, 1), holding.getSequences().get(1).getEndDate());

        Assertions.assertEquals(sequenceToAdd1, holding.getSequences().get(2));
    }

    @Test
    @DisplayName("test ajout de lacune dans état de collection vide")
    void addLacuneOnEmptyHolding() {
        Holding holding = new Holding("41133793901");
        Sequence sequenceToAdd = new SequenceLacune(1989, Calendar.FEBRUARY, 28, "", "");
        sequenceToAdd.setEndDate(1989, Calendar.MARCH, 31);
        holding.addSequence(sequenceToAdd);

        Assertions.assertEquals(1, holding.getErrorSequences().size());
        Assertions.assertEquals("Impossible d'ajouter la lacune dans un état de collection vide", holding.getErrorSequences().get(0).getMessage());
    }

    @Test
    @DisplayName("test ajout de lacune mal placée")
    void addLacuneMisplaced() {
        Holding holding = new Holding("41133793901");
        Sequence sequenceToAdd = new SequenceLacune(1989, Calendar.FEBRUARY, 28, "", "");
        sequenceToAdd.setEndDate(1989, Calendar.MARCH, 31);
        Sequence sequence1 = new SequenceContinue(1990, Calendar.JANUARY, 1, "", "");
        holding.addSequence(sequence1);

        holding.addSequence(sequenceToAdd);
        Assertions.assertEquals(1, holding.getErrorSequences().size());
        Assertions.assertEquals("Lacune en dehors de l'état de collection", holding.getErrorSequences().get(0).getMessage());
    }

    @Test
    @DisplayName("test ajout lacune avec découpage de séquence existante")
    void addLacuneWithCutSequence() {
        Holding holding = new Holding("41133793901");
        Sequence lacune = new SequenceLacune(1991, Calendar.FEBRUARY, 28, "", "");
        lacune.setEndDate(1991, Calendar.MARCH, 31);

        Sequence sequence = new SequenceContinue(1990, Calendar.JANUARY, 1, "", "");
        sequence.setEndDate(1992, Calendar.APRIL, 20);
        holding.addSequence(sequence);

        holding.addSequence(lacune);
        Assertions.assertEquals(3, holding.getSequences().size());
        Assertions.assertEquals(new GregorianCalendar(1990, Calendar.JANUARY, 1), holding.getSequences().get(0).getStartDate());
        Assertions.assertEquals(new GregorianCalendar(1991, Calendar.FEBRUARY, 28), holding.getSequences().get(0).getEndDate());
        Assertions.assertEquals(new GregorianCalendar(1991, Calendar.FEBRUARY, 28), holding.getSequences().get(1).getStartDate());
        Assertions.assertEquals(new GregorianCalendar(1991, Calendar.MARCH, 31), holding.getSequences().get(1).getEndDate());
        Assertions.assertEquals(new GregorianCalendar(1991, Calendar.MARCH, 31), holding.getSequences().get(2).getStartDate());
        Assertions.assertEquals(new GregorianCalendar(1992, Calendar.APRIL, 20), holding.getSequences().get(2).getEndDate());
    }

    @Test
    @DisplayName("test ajout lacune avec découpage de séquence lacunaire existante")
    void addLacuneWithCutSequenceLacune() {
        Holding holding = new Holding("41133793901");
        Sequence sequenceToAdd = new SequenceLacune(1991, Calendar.FEBRUARY, 28, "", "");
        sequenceToAdd.setEndDate(1991, Calendar.MARCH, 31);

        Sequence sequence1 = new SequenceContinue(1990, Calendar.JANUARY, 1, "", "");
        sequence1.setEndDate(1992, Calendar.OCTOBER, 20);
        holding.addSequence(sequence1);

        holding.addSequence(sequenceToAdd);

        Sequence sequenceToAdd2 = new SequenceLacune(1991, Calendar.APRIL, 1, "", "");
        sequenceToAdd2.setEndDate(1991, Calendar.APRIL, 30);

        holding.addSequence(sequenceToAdd2);

        Assertions.assertEquals(5, holding.getSequences().size());
        Assertions.assertEquals(new GregorianCalendar(1990, Calendar.JANUARY, 1), holding.getSequences().get(0).getStartDate());
        Assertions.assertEquals(new GregorianCalendar(1991, Calendar.FEBRUARY, 28), holding.getSequences().get(0).getEndDate());
        Assertions.assertEquals(new GregorianCalendar(1991, Calendar.FEBRUARY, 28), holding.getSequences().get(1).getStartDate());
        Assertions.assertEquals(new GregorianCalendar(1991, Calendar.MARCH, 31), holding.getSequences().get(1).getEndDate());
        Assertions.assertEquals(new GregorianCalendar(1991, Calendar.MARCH, 31), holding.getSequences().get(2).getStartDate());
        Assertions.assertEquals(new GregorianCalendar(1991, Calendar.APRIL, 1), holding.getSequences().get(2).getEndDate());
        Assertions.assertEquals(new GregorianCalendar(1991, Calendar.APRIL, 1), holding.getSequences().get(3).getStartDate());
        Assertions.assertEquals(new GregorianCalendar(1991, Calendar.APRIL, 30), holding.getSequences().get(3).getEndDate());
        Assertions.assertEquals(new GregorianCalendar(1991, Calendar.APRIL, 30), holding.getSequences().get(4).getStartDate());
        Assertions.assertEquals(new GregorianCalendar(1992, Calendar.OCTOBER, 20), holding.getSequences().get(4).getEndDate());
    }
}
