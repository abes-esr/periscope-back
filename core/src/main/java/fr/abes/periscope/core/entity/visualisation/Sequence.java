package fr.abes.periscope.core.entity.visualisation;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Calendar;

@Data
@AllArgsConstructor
@NoArgsConstructor
public abstract class Sequence implements Cloneable {
    protected Calendar startDate;
    protected String startVolume;
    protected String startNumero;

    protected Calendar endDate;
    protected String endVolume;
    protected String endNumero;
    protected String note;

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

    @Override
    public Object clone() {
        try {
            return super.clone();
        }
        catch (CloneNotSupportedException e){
            throw new InternalError();
        }
    }
}
