package fr.abes.periscope.core.entity.visualisation;

import fr.abes.periscope.core.entity.EnumMonth;
import lombok.Data;

@Data
public class Bloc {
    private String volume;
    private String numero;
    private Integer annee;
    private Integer jour;
    private EnumMonth mois;
}
