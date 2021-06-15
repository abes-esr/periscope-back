package fr.abes.periscope.core.entity.visualisation;

import java.util.Calendar;

public class SequenceError extends Sequence {

    private String message;

    public SequenceError(Integer startYear, Integer startMonth, Integer startDay, Integer endYear, Integer endMonth, Integer endDay, String message) {
        super(startYear,startMonth,startDay,endYear,endMonth,endDay);
        this.message = message;
    }

    public SequenceError(Integer startYear, Integer startMonth, Integer startDay, String message) {
        super(startYear, startMonth, startDay);
        this.message = message;
    }

    public SequenceError(Sequence sequence, String message) {
        super(sequence.getStartDate().get(Calendar.YEAR), sequence.getStartDate().get(Calendar.MONTH), sequence.getStartDate().get(Calendar.DAY_OF_MONTH),sequence.getEndDate().get(Calendar.YEAR), sequence.getEndDate().get(Calendar.MONTH), sequence.getEndDate().get(Calendar.DAY_OF_MONTH));
        this.message = message;
    }

    public String getMessage() {
        return message;
    }

    @Override
    public boolean equals(Object obj) {
        return super.equals(obj);
    }

    @Override
    public String toString() {
        return "SequenceError {"+ "startDate="+ startDate.getTime() +", endDate=" + endDate.getTime() +", message=" + message +"}";
    }
}
