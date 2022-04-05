package fr.abes.periscope.web.dto;


import fr.abes.periscope.web.util.TYPE_SEQUENCE;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

class NoticeVisuWebDtoTest {
    @Test
    void testAddHoldingAgregee() {
        NoticeVisuWebDto notice = new NoticeVisuWebDto(1990, 2020);
        HoldingWebDto holding1 = new HoldingWebDto();
        holding1.addSequence(new SequenceWebDto(1990, 1995, TYPE_SEQUENCE.CONTINUE, "341725201"));
        holding1.addSequence(new SequenceWebDto(1995, 1996, TYPE_SEQUENCE.LACUNE, "341725201"));
        holding1.addSequence(new SequenceWebDto(1996, 2000, TYPE_SEQUENCE.CONTINUE, "341725201"));
        holding1.addSequence(new SequenceWebDto(2005, 2020, TYPE_SEQUENCE.CONTINUE, "341725201"));

        notice.addHolding(holding1);

        HoldingWebDto holding2 = new HoldingWebDto();
        holding2.addSequence(new SequenceWebDto(1990, 1992, TYPE_SEQUENCE.CONTINUE, "341725201"));
        holding2.addSequence(new SequenceWebDto(1992, 1994, TYPE_SEQUENCE.LACUNE, "341725201"));
        holding2.addSequence(new SequenceWebDto(1994, 1994, TYPE_SEQUENCE.CONTINUE, "341725201"));
        holding2.addSequence(new SequenceWebDto(2007, 2020, TYPE_SEQUENCE.CONTINUE, "341725201"));

        notice.addHolding(holding2);

        notice.addHoldingAgregee();

        Assertions.assertEquals(3, notice.getHoldingWebDtoList().size());
        Assertions.assertEquals("1", notice.getHoldingWebDtoList().get(0).getSequencesList().get(0).getRcr());
        Assertions.assertEquals(1990, notice.getHoldingWebDtoList().get(0).getSequencesList().get(0).getAnneeDebut());
        Assertions.assertEquals(2000, notice.getHoldingWebDtoList().get(0).getSequencesList().get(0).getAnneeFin());
        Assertions.assertEquals(TYPE_SEQUENCE.CONTINUE, notice.getHoldingWebDtoList().get(0).getSequencesList().get(0).getTypeSequence());
        Assertions.assertEquals(2000, notice.getHoldingWebDtoList().get(0).getSequencesList().get(1).getAnneeDebut());
        Assertions.assertEquals(2005, notice.getHoldingWebDtoList().get(0).getSequencesList().get(1).getAnneeFin());
        Assertions.assertEquals(TYPE_SEQUENCE.LACUNE, notice.getHoldingWebDtoList().get(0).getSequencesList().get(1).getTypeSequence());
        Assertions.assertEquals(2005, notice.getHoldingWebDtoList().get(0).getSequencesList().get(2).getAnneeDebut());
        Assertions.assertEquals(2020, notice.getHoldingWebDtoList().get(0).getSequencesList().get(2).getAnneeFin());
        Assertions.assertEquals(TYPE_SEQUENCE.CONTINUE, notice.getHoldingWebDtoList().get(0).getSequencesList().get(2).getTypeSequence());
    }


}
