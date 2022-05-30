package fr.abes.periscope.web.dto.criterion;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonTypeName;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.validation.constraints.NotNull;
import java.util.List;
import java.util.Set;

/**
 * Représente un critère de recherche par statut de bibliothèque au format JSON de l'API
 */
@Getter
@Setter
@NoArgsConstructor
@JsonTypeName(CriterionTypeName.CRITERION_STATUT_BIB)
public class CriterionStatutBibWebDto extends CriterionWebDto {
    public static final String STATUT_PROPERTY = "statut";
    public static final String STATUT_OPERATOR_PROPERTY = "statut_operator";

    @JsonProperty(value= STATUT_PROPERTY)
    @NotNull(message = "Le statut de la bibliothèque ne doit pas être null")
    private List<String> statut;

    @JsonProperty(value= STATUT_OPERATOR_PROPERTY)
    @NotNull(message = "La liste des connecteurs logiques ne doit pas être nulle")
    private List<String> statutsOperators;

}
