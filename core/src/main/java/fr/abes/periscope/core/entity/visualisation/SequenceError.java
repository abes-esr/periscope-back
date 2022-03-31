package fr.abes.periscope.core.entity.visualisation;

public class SequenceError extends Sequence {

    private String message;

    public SequenceError(Integer startYear, Integer endYear, String message) {
        super(startYear,endYear);
        this.message = message;
    }

    public SequenceError(Integer startYear, String message) {
        super(startYear, startYear);
        this.message = message;
    }

    public SequenceError(Sequence sequence, String message) {
        super(sequence.getStartDate(), sequence.getEndDate());
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
        return "SequenceError {"+ "startDate="+ startDate +", endDate=" + endDate +", message=" + message +"}";
    }
}
