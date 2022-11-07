package fr.abes.periscope.core.entity.visualisation;

public class SequenceLacune extends Sequence {
    protected String volume;
    protected String numero;


    public SequenceLacune(Integer startYear, Integer endYear, String volume, String numero) {
        super(startYear, endYear);
        setVolume(volume);
        setNumero(numero);
    }

    public SequenceLacune(Integer startYear, String volume, String numero) {
        super(startYear, startYear);
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

    @Override
    public boolean equals(Object obj) {
        return super.equals(obj);
    }

    @Override
    public String toString() {
        return "SequenceLacune {" + "startDate=" + startDate + ", endDate=" + endDate + ", volume=" + volume + ", numero=" + numero + "}";
    }
}
