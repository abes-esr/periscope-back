package fr.abes.periscope.core.entity.visualisation;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public abstract class Sequence implements Cloneable {
    protected Integer startDate;
    protected Integer endDate;
    private boolean closedInterval = false;

    public Sequence(Integer startDate) {
        this.startDate = startDate;
        this.closedInterval = false;
    }

    public Sequence(Integer startDate, Integer endDate) {
        this.startDate = startDate;
        this.endDate = endDate;
        this.closedInterval = true;
    }


    public void setEndDate(Integer endDate) {
        if (endDate == null) {
            //pas de date de fin
            return;
        }

        this.endDate = endDate;
        this.closedInterval = true;

        this.checkDate();
    }

    public boolean isClosedInterval() {
        return this.closedInterval;
    }

    /**
     * Permet de vérifier si la date de début et la date de fin de la séquence ne sont pas inversée et les intervertit si elles le sont
     */
    private void checkDate() {
        if (startDate != null && endDate != null) {
            if (startDate > endDate) {
                // On échange les dates
                Integer temp = endDate;
                endDate = startDate;
                startDate = temp;
            }
        }
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

        return startDate.equals(((Sequence) obj).startDate) && endDate.equals(((Sequence) obj).endDate);
    }

    @Override
    public String toString() {
        if (isClosedInterval()) {
            return "Sequence {" + "startDate=" + startDate + ", endDate=" + endDate + "}";
        } else {
            return "Sequence {" + "startDate=" + startDate + ", endDate=undefined }";
        }
    }

    @Override
    public Object clone() {
        try {
            return super.clone();
        } catch (CloneNotSupportedException e) {
            throw new InternalError();
        }
    }
}
