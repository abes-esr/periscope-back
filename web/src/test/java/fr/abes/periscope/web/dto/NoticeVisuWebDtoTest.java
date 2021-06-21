package fr.abes.periscope.web.dto;


import fr.abes.periscope.web.util.TYPE_SEQUENCE;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.text.ParseException;

public class NoticeVisuWebDtoTest {
    @Test
    void testAddHoldingAgregee() throws ParseException {
        NoticeVisuWebDto notice = new NoticeVisuWebDto("1990-01-01", "2020-10-12");
        HoldingWebDto holding1 = new HoldingWebDto();
        holding1.addSequence(new SequenceWebDto("1990-01-01", "1995-10-12", TYPE_SEQUENCE.CONTINUE, "341725201"));
        holding1.addSequence(new SequenceWebDto("1995-10-12", "1996-01-12", TYPE_SEQUENCE.LACUNE, "341725201"));
        holding1.addSequence(new SequenceWebDto("1996-01-12", "2000-02-01", TYPE_SEQUENCE.CONTINUE, "341725201"));
        holding1.addSequence(new SequenceWebDto("2005-01-01", "2020-10-12", TYPE_SEQUENCE.CONTINUE, "341725201"));

        notice.addHolding(holding1);

        HoldingWebDto holding2 = new HoldingWebDto();
        holding2.addSequence(new SequenceWebDto("1990-01-01", "1992-10-12", TYPE_SEQUENCE.CONTINUE, "341725201"));
        holding2.addSequence(new SequenceWebDto("1992-10-12", "1994-01-12", TYPE_SEQUENCE.LACUNE, "341725201"));
        holding2.addSequence(new SequenceWebDto("1994-01-12", "1994-03-12", TYPE_SEQUENCE.CONTINUE, "341725201"));
        holding2.addSequence(new SequenceWebDto("2007-01-01", "2020-10-12", TYPE_SEQUENCE.CONTINUE, "341725201"));

        notice.addHolding(holding2);

        notice.addHoldingAgregee();

        Assertions.assertEquals(3, notice.getHoldingWebDtoList().size());
        Assertions.assertEquals("1", notice.getHoldingWebDtoList().get(0).getSequencesList().get(0).getRcr());
        Assertions.assertEquals("1990-01-01", notice.getHoldingWebDtoList().get(0).getSequencesList().get(0).getDateDebut());
        Assertions.assertEquals("2000-02-01", notice.getHoldingWebDtoList().get(0).getSequencesList().get(0).getDateFin());
        Assertions.assertEquals(TYPE_SEQUENCE.CONTINUE, notice.getHoldingWebDtoList().get(0).getSequencesList().get(0).getTypeSequence());
        Assertions.assertEquals("2000-02-01", notice.getHoldingWebDtoList().get(0).getSequencesList().get(1).getDateDebut());
        Assertions.assertEquals("2005-01-01", notice.getHoldingWebDtoList().get(0).getSequencesList().get(1).getDateFin());
        Assertions.assertEquals(TYPE_SEQUENCE.LACUNE, notice.getHoldingWebDtoList().get(0).getSequencesList().get(1).getTypeSequence());
        Assertions.assertEquals("2005-01-01", notice.getHoldingWebDtoList().get(0).getSequencesList().get(2).getDateDebut());
        Assertions.assertEquals("2020-10-12", notice.getHoldingWebDtoList().get(0).getSequencesList().get(2).getDateFin());
        Assertions.assertEquals(TYPE_SEQUENCE.CONTINUE, notice.getHoldingWebDtoList().get(0).getSequencesList().get(2).getTypeSequence());
    }
}
