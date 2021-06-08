package fr.abes.periscope.core.entity.visualisation;

import lombok.Getter;
import lombok.Setter;

import java.util.Calendar;

@Getter
@Setter
public class SequenceError extends Sequence {
    private String message;

    public SequenceError(Calendar startDate) {
        super(startDate);
        message = "";
    }

    public SequenceError(Sequence sequence, String message) {
        this.startDate = sequence.getStartDate();
        this.endDate = sequence.getEndDate();
        this.startNumero = sequence.getStartNumero();
        this.endNumero = sequence.getEndNumero();
        this.startVolume = sequence.getStartVolume();
        this.endVolume = sequence.getEndVolume();
        this.message = message;
    }
}
