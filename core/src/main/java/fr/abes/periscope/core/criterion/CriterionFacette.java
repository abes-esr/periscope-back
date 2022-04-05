package fr.abes.periscope.core.criterion;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

import java.util.List;

@Getter
@Setter
@AllArgsConstructor
public class CriterionFacette {
    /** Nom de la zone sur laquelle la facette porte */
    private String zone;
    /** Valeur à rechercher dans la facette */
    private List<String> valeurs;
}
