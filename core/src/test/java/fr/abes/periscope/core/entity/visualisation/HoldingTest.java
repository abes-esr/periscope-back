package fr.abes.periscope.core.entity.visualisation;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.Calendar;
import java.util.GregorianCalendar;

@SpringBootTest(classes = Holding.class)
public class HoldingTest {

    private Holding holding = new Holding();

    @Test
    void findCloserTest() {
        Holding holding = new Holding();
        Sequence sequenceToAdd = new SequenceLacune();
        sequenceToAdd.setStartDate(new GregorianCalendar(1989, Calendar.FEBRUARY, 28));
        Assertions.assertNull(holding.findCloser(sequenceToAdd));

        Sequence sequence1 = new SequenceContinue();
        sequence1.setStartDate(new GregorianCalendar(1990, Calendar.JANUARY, 1));
        holding.addSequence(sequence1);
        Assertions.assertEquals(sequence1, holding.findCloser(sequenceToAdd));

        Sequence sequence2 = new SequenceContinue();
        sequence2.setStartDate(new GregorianCalendar(1995, Calendar.JANUARY, 31));
        holding.addSequence(sequence2);

        sequenceToAdd.setStartDate(new GregorianCalendar(1996, Calendar.MARCH, 31));
        Assertions.assertEquals(sequence2, holding.findCloser(sequenceToAdd));
    }

    @Test
    @DisplayName("test ajout de lacune dans état de collection vide")
    void addLacuneOnEmptyHolding() {
        Holding holding = new Holding();
        Sequence sequenceToAdd = new SequenceLacune();
        sequenceToAdd.setStartDate(new GregorianCalendar(1989, Calendar.FEBRUARY, 28));
        sequenceToAdd.setEndDate(new GregorianCalendar(1989, Calendar.MARCH, 31));
        holding.addSequence(sequenceToAdd);
        Assertions.assertEquals("Impossible d'ajouter la lacune " + sequenceToAdd.toString() + " dans un état de collection vide", holding.getErreurs().get(0));
        Assertions.assertNull(holding.getSequences());
    }

    @Test
    @DisplayName("test ajout de lacune mal placée")
    void addLacuneMisplaced(){
        Holding holding = new Holding();
        Sequence sequenceToAdd = new SequenceLacune();
        sequenceToAdd.setStartDate(new GregorianCalendar(1989, Calendar.FEBRUARY, 28));
        sequenceToAdd.setEndDate(new GregorianCalendar(1989, Calendar.MARCH, 31));
        Sequence sequence1 = new SequenceContinue();
        sequence1.setStartDate(new GregorianCalendar(1990, Calendar.JANUARY, 1));
        holding.addSequence(sequence1);

        holding.addSequence(sequenceToAdd);
        Assertions.assertEquals("Lacune : " + sequenceToAdd.toString() + " en dehors de l'état de collection", holding.getErreurs().get(0));
        Assertions.assertEquals(1, holding.getSequences().size());
    }

    @Test
    @DisplayName("test ajout lacune avec découpage de séquence existante")
    void addLacuneWithCutSequence() {
        Holding holding = new Holding();
        Sequence sequenceToAdd = new SequenceLacune();
        sequenceToAdd.setStartDate(new GregorianCalendar(1991, Calendar.FEBRUARY, 28));
        sequenceToAdd.setEndDate(new GregorianCalendar(1991, Calendar.MARCH, 31));

        Sequence sequence1 = new SequenceContinue();
        sequence1.setStartDate(new GregorianCalendar(1990, Calendar.JANUARY, 1));
        sequence1.setEndDate(new GregorianCalendar(1992, Calendar.APRIL, 20));
        holding.addSequence(sequence1);

        holding.addSequence(sequenceToAdd);
        Assertions.assertEquals(3, holding.getSequences().size());
        Assertions.assertEquals(new GregorianCalendar(1990, Calendar.JANUARY, 1), holding.getSequences().get(0).getStartDate());
        Assertions.assertEquals(new GregorianCalendar(1991, Calendar.FEBRUARY, 27), holding.getSequences().get(0).getEndDate());
        Assertions.assertEquals(new GregorianCalendar(1991, Calendar.FEBRUARY, 28), holding.getSequences().get(1).getStartDate());
        Assertions.assertEquals(new GregorianCalendar(1991, Calendar.MARCH, 31), holding.getSequences().get(1).getEndDate());
        Assertions.assertEquals(new GregorianCalendar(1991, Calendar.APRIL, 1), holding.getSequences().get(2).getStartDate());
        Assertions.assertEquals(new GregorianCalendar(1992, Calendar.APRIL, 20), holding.getSequences().get(2).getEndDate());
    }

    @Test
    @DisplayName("test ajout lacune avec découpage de séquence lacunaire existante")
    void addLacuneWithCutSequenceLacune() {
        Holding holding = new Holding();
        Sequence sequenceToAdd = new SequenceLacune();
        sequenceToAdd.setStartDate(new GregorianCalendar(1991, Calendar.FEBRUARY, 28));
        sequenceToAdd.setEndDate(new GregorianCalendar(1991, Calendar.MARCH, 31));

        Sequence sequence1 = new SequenceContinue();
        sequence1.setStartDate(new GregorianCalendar(1990, Calendar.JANUARY, 1));
        sequence1.setEndDate(new GregorianCalendar(1992, Calendar.OCTOBER, 20));
        holding.addSequence(sequence1);

        holding.addSequence(sequenceToAdd);

        Sequence sequenceToAdd2 = new SequenceLacune();
        sequenceToAdd2.setStartDate(new GregorianCalendar(1991, Calendar.APRIL, 1));
        sequenceToAdd2.setEndDate(new GregorianCalendar(1991, Calendar.APRIL, 30));

        holding.addSequence(sequenceToAdd2);

        Assertions.assertEquals(4, holding.getSequences().size());
        Assertions.assertEquals(new GregorianCalendar(1990, Calendar.JANUARY, 1), holding.getSequences().get(0).getStartDate());
        Assertions.assertEquals(new GregorianCalendar(1991, Calendar.FEBRUARY, 27), holding.getSequences().get(0).getEndDate());
        Assertions.assertEquals(new GregorianCalendar(1991, Calendar.FEBRUARY, 28), holding.getSequences().get(1).getStartDate());
        Assertions.assertEquals(new GregorianCalendar(1991, Calendar.MARCH, 31), holding.getSequences().get(1).getEndDate());
        Assertions.assertEquals(new GregorianCalendar(1991, Calendar.APRIL, 1), holding.getSequences().get(2).getStartDate());
        Assertions.assertEquals(new GregorianCalendar(1991, Calendar.APRIL, 30), holding.getSequences().get(2).getEndDate());
        Assertions.assertEquals(new GregorianCalendar(1991, Calendar.MAY, 1), holding.getSequences().get(3).getStartDate());
        Assertions.assertEquals(new GregorianCalendar(1992, Calendar.OCTOBER, 20), holding.getSequences().get(3).getEndDate());
    }
}
