package fr.abes.periscope.core.entity.visualisation;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Calendar;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class SequenceLacune extends Sequence {
    protected String volume;
    protected String numero;

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
        return "SequenceLacune {"+ "startDate="+ startDate.getTime() +"}";
    }
}
