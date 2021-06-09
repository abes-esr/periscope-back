package fr.abes.periscope.core.entity.visualisation;

import java.time.Period;
import java.util.Calendar;

public class SequenceLacune extends Sequence {
    protected String volume;
    protected String numero;
    private boolean updateToFrequency = false;

    public SequenceLacune(Integer startYear, Integer startMonth, Integer startDay, Integer endYear, Integer endMonth, Integer endDay, String volume, String numero) {
        super(startYear, startMonth, startDay, endYear, endMonth, endDay);
        setVolume(volume);
        setNumero(numero);
    }

    public SequenceLacune(Integer startYear, Integer startMonth, Integer startDay, Integer endYear, Integer endMonth, Integer endDay, String volume, String numero, boolean containsFrequency) {
        super(startYear, startMonth, startDay, endYear, endMonth, endDay);
        setVolume(volume);
        setNumero(numero);
        this.updateToFrequency = containsFrequency;
    }

    public SequenceLacune(Integer startYear, Integer startMonth, Integer startDay, String volume, String numero) {
        super(startYear, startMonth, startDay);
        setVolume(volume);
        setNumero(numero);
    }

    public String getVolume() {
        return this.volume;
    }

    public void setVolume(String value) {
        if (value == null || value.isEmpty()) {
            this.volume = "Non renseigné";
        } else {
            this.volume = value;
        }
    }

    public String getNumero() {
        return this.numero;
    }

    public void setNumero(String value) {
        if (value == null || value.isEmpty()) {
            this.numero = "Non renseigné";
        } else {
            this.numero = value;
        }
    }

    public boolean updateToFrequency(Period frequency) {
        if (!this.updateToFrequency) {
            endDate.add(Calendar.DAY_OF_MONTH, frequency.getDays());
            endDate.add(Calendar.MONTH, frequency.getMonths());
            endDate.add(Calendar.YEAR, frequency.getYears());
            this.updateToFrequency = true;
        }
        return this.updateToFrequency;
    }

    @Override
    public boolean equals(Object obj) {
        return super.equals(obj);
    }

    @Override
    public String toString() {
        return "SequenceLacune {" + "startDate=" + startDate.getTime() + ", endDate=" + endDate.getTime() + ", volume=" + volume + ", numero=" + numero + "}";
    }
}
