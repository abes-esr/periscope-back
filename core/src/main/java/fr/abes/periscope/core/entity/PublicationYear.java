package fr.abes.periscope.core.entity;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class PublicationYear {

    private int year;

    private int confidenceIndex;

    public PublicationYear() {
        this.confidenceIndex = 0;
    }

    public PublicationYear(int candidateYear, int candidateConfidenceIndex) {
        this.year = candidateYear;
        this.confidenceIndex = candidateConfidenceIndex;
    }

    @Override
    public String toString() {
        return "PublicationYear {year="+ year +"("+ confidenceIndex +")}";
    }
}
