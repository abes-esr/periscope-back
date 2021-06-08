package fr.abes.periscope.core.entity.visualisation;

import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
public class SequenceContinue extends Sequence {
    public SequenceContinue(Sequence sequence) {
        this.startDate = sequence.getStartDate();
        this.endDate = sequence.getEndDate();
        this.startNumero = sequence.getStartNumero();
        this.endNumero = sequence.getEndNumero();
        this.startVolume = sequence.getStartVolume();
        this.endVolume = sequence.getEndVolume();
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == null) {
            return false;
        }

        if (this == obj) {
            return true;
        }

        if (getClass() != obj.getClass()) {
            return false;
        }

        return startDate.equals(((Sequence) obj).startDate) ;
    }

    @Override
    public String toString() {
        String str = "";
        if (this.startDate != null) {
            str += "Sequence {" + "startDate=" + startDate.getTime();
            if (this.endDate != null) {
                str += ", endDate=" + endDate.getTime() + "}";
            }
        }
        return str;
    }

}
