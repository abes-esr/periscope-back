package fr.abes.periscope.core.entity.visualisation;

import lombok.Getter;

import java.util.Calendar;

@Getter
public class SequenceContinue extends Sequence {
    protected String startVolume;
    protected String startNumero;

    protected String endVolume;
    protected String endNumero;

    public SequenceContinue(Integer startYear, String startVolume, String startNumero, boolean ouvert) {
        super(startYear);
        if (ouvert) {
            Calendar now = Calendar.getInstance();
            setEndDate(now.get(Calendar.YEAR));
        } else {
            setEndDate(startYear);
        }

        setStartVolume(startVolume);
        setStartNumero(startNumero);
        setEndVolume(startVolume);
        setEndNumero(startNumero);
    }

    public SequenceContinue(Integer startDate, String startVolume, String startNumero, Integer endYear, String endVolume, String endNumero) {
        super(startDate, endYear);
        setStartVolume(startVolume);
        setStartNumero(startNumero);
        setEndVolume(endVolume);
        setEndNumero(endNumero);
    }

    public void setStartDate(Integer startYear, String volume, String numero) {
        this.setStartDate(startYear);
        setStartVolume(volume);
        setStartNumero(numero);
    }

    public void setStartVolume(String value) {
        if (value == null || value.isEmpty()) {
            this.startVolume = "Non renseigné";
        } else {
            this.startVolume = value;
        }
    }

    public void setStartNumero(String value) {
        if (value == null || value.isEmpty()) {
            this.startNumero = "Non renseigné";
        } else {
            this.startNumero = value;
        }
    }

    public void setEndVolume(String value) {
        if (value == null || value.isEmpty()) {
            this.endVolume = "Non renseigné";
        } else {
            this.endVolume = value;
        }
    }

    public void setEndNumero(String value) {
        if (value == null || value.isEmpty()) {
            this.endNumero = "Non renseigné";
        } else {
            this.endNumero = value;
        }
    }

    @Override
    public boolean equals(Object obj) {
        return super.equals(obj);
    }

    @Override
    public String toString() {
        return "SequenceContinue {" + "startDate=" + startDate + ", endDate=" + endDate + ", startVolume=" + startVolume + ", endVolume=" + endVolume + "}";
    }

}
