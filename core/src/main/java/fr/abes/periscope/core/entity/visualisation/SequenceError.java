package fr.abes.periscope.core.entity.visualisation;

public class SequenceError extends Sequence {
    public SequenceError(Sequence sequence) {
        this.startDate = sequence.getStartDate();
        this.endDate = sequence.getEndDate();
        this.startNumero = sequence.getStartNumero();
        this.endNumero = sequence.getEndNumero();
        this.startVolume = sequence.getStartVolume();
        this.endVolume = sequence.getEndVolume();
    }
}
