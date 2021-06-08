package fr.abes.periscope.core.entity.visualisation;

import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Calendar;

@Data
@NoArgsConstructor
public class SequenceEmpty extends Sequence {
    public SequenceEmpty(Calendar startDate){
        super(startDate);
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
    public String toString(){
        String str = "Sequence vide {" + "startDate=" + startDate.getTime();
        if (this.endDate != null) {
            str += ", endDate=" + endDate.getTime() + "}";
        }
        return str;
    }
}
