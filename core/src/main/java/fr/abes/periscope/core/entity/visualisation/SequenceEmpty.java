package fr.abes.periscope.core.entity.visualisation;

public class SequenceEmpty extends Sequence {

    public SequenceEmpty(Integer startYear, Integer endYear) {
        super(startYear,endYear);
    }

    @Override
    public boolean equals(Object obj) {
        return super.equals(obj);
    }

    @Override
    public String toString() {
        return "SequenceEmpty {"+ "startDate="+ startDate +", endDate=" + endDate +"}";
    }
}
