package fr.abes.periscope.core.entity.visualisation;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Calendar;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class SequenceContinue extends Sequence {

    protected String texteEtatCollectionZone;
    protected String note;
    protected String mentionDeLacune;

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
        return "Sequence {"+ "startDate="+ startDate.getTime() +", endDate="+ endDate.getTime() +"}";
    }

}
