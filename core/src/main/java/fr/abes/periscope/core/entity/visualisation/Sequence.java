package fr.abes.periscope.core.entity.visualisation;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.Locale;

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

    public Sequence(Calendar startDate) {
        this.startDate = startDate;
        this.endDate = startDate;

        this.startNumero = "";
        this.startVolume = "";
        this.endNumero = "";
        this.endVolume = "" ;
        this.note = "";
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

    /**
     * Méthode de calcul de la date de fin d'une séquence à partir de la périodicité de la ressource
     * @param frequency
     */
    public void calculEndDateFromFrequency(String frequency) {
        if (endDate == null) {
            Calendar dateFin = new GregorianCalendar(startDate.get(Calendar.YEAR), startDate.get(Calendar.MONTH), startDate.get(Calendar.DAY_OF_MONTH));
            this.setEndDate(dateFin);
            switch (frequency.toUpperCase(Locale.ROOT)) {
                case "A":
                    this.getEndDate().add(Calendar.DAY_OF_MONTH, 1);
                    break;
                case "B":
                    this.getEndDate().add(Calendar.DAY_OF_MONTH, 3);
                    break;
                case "C":
                    this.getEndDate().add(Calendar.WEEK_OF_MONTH, 1);
                    break;
                case "D":
                case "E":
                    this.getEndDate().add(Calendar.WEEK_OF_MONTH, 2);
                    break;
                case "F":
                    this.getEndDate().add(Calendar.MONTH, 1);
                    break;
                case "G":
                    this.getEndDate().add(Calendar.MONTH, 2);
                    break;
                case "H":
                    this.getEndDate().add(Calendar.MONTH, 3);
                    break;
                case "I":
                    this.getEndDate().add(Calendar.MONTH, 4);
                    break;
                case "J":
                    this.getEndDate().add(Calendar.MONTH, 6);
                    break;
                case "K":
                    this.getEndDate().add(Calendar.YEAR, 1);
                    break;
                case "L":
                    this.getEndDate().add(Calendar.YEAR, 2);
                    break;
                case "M":
                    this.getEndDate().add(Calendar.YEAR, 3);
                    break;
                case "N":
                    this.getEndDate().add(Calendar.DAY_OF_MONTH, 2);
                    break;
                case "O":
                    this.getEndDate().add(Calendar.DAY_OF_MONTH, 10);
                    break;
                default:

            }
        }
    }
}
