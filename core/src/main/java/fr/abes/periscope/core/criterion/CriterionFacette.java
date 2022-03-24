package fr.abes.periscope.core.criterion;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@AllArgsConstructor
public class CriterionFacette {
    /** Nom de la zone sur laquelle la facette porte */
    private String zone;
    /** Valeur à rechercher dans la facette */
    private String valeur;
}
