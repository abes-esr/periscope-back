package fr.abes.periscope.core.entity.visualisation;

public class SequenceEmpty extends Sequence {

    public SequenceEmpty(Integer startYear, Integer startMonth, Integer startDay, Integer endYear, Integer endMonth, Integer endDay) {
        super(startYear,startMonth,startDay,endYear,endMonth,endDay);
    }

    @Override
    public boolean equals(Object obj) {
        return super.equals(obj);
    }

    @Override
    public String toString() {
        return "SequenceEmpty {"+ "startDate="+ startDate.getTime() +", endDate=" + endDate.getTime() +"}";
    }
}
