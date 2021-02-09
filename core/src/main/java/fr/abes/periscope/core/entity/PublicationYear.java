package fr.abes.periscope.core.entity;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class PublicationYear {

    private Integer year;

    private Integer confidenceIndex;

    public PublicationYear() {
        this.year=null;
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
