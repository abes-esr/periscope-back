package fr.abes.periscope.core.entity.visualisation;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.Calendar;
import java.util.GregorianCalendar;

@SpringBootTest(classes = Holding.class)
public class HoldingTest {

    private Holding holding = new Holding();

    @BeforeEach
    void init() {
        //Création d'un holding de base pour les tests
        Sequence sequence1 = new Sequence();
        sequence1.setBlocDebut(new BlocDebut(new GregorianCalendar(1960, Calendar.JANUARY, 1), "", ""));
        sequence1.setBlocFin(new BlocFin(new GregorianCalendar(1965, Calendar.DECEMBER, 31), "", ""));

        Sequence sequence2 = new Sequence();
        sequence2.setBlocDebut(new BlocDebut(new GregorianCalendar(1967, Calendar.JANUARY, 1), "", ""));
        sequence2.setBlocFin(new BlocFin(new GregorianCalendar(1975, Calendar.DECEMBER, 31), "", ""));

        Sequence sequence3 = new Sequence();
        sequence3.setBlocDebut(new BlocDebut(new GregorianCalendar(1977, Calendar.JANUARY, 1), "", ""));
        sequence3.setBlocFin(new BlocFin(new GregorianCalendar(1988, Calendar.DECEMBER, 31), "", ""));

        this.holding.addSequence(sequence3);
        this.holding.addSequence(sequence1);
        this.holding.addSequence(sequence2);

        Lacune lacune = new Lacune();
        lacune.addBloc(new BlocDebut(new GregorianCalendar(1962, Calendar.FEBRUARY, 28), "", ""));
        lacune.addBloc(new BlocDebut(new GregorianCalendar(1969, Calendar.OCTOBER, 15), "", ""));
        lacune.addBloc(new BlocDebut(new GregorianCalendar(1979, Calendar.SEPTEMBER, 12), "", ""));
        lacune.addBloc(new BlocDebut(new GregorianCalendar(1962, Calendar.MARCH, 10), "", ""));
        lacune.addBloc(new BlocDebut(new GregorianCalendar(1962, Calendar.APRIL, 9), "", ""));

        holding.setLacune(lacune);

    }
    @Test
    @DisplayName("test ordonnancement des séquences")
    void testOrdoSequence() {
        this.holding.getSequencesOrdonnees();
    }
}
