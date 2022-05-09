package fr.abes.periscope.web.dto.criterion;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonTypeName;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.validation.constraints.NotNull;
import java.util.List;

/**
 * Représente un critère de recherche par code pays au format JSON de l'API
 */
@Getter @Setter
@NoArgsConstructor
@JsonTypeName(CriterionTypeName.CRITERION_COUNTRIES)
public class CriterionCountryWebDto extends CriterionWebDto {

    public static final String COUNTRIES_PROPERTY = "countries";
    public static final String COUNTRIES_OPERATOR_PROPERTY = "countries_operator";

    @JsonProperty(value= COUNTRIES_PROPERTY)
    @NotNull(message = "La liste des codes pays ne doit pas être nulle")
    private List<String> countries;

    @JsonProperty(value= COUNTRIES_OPERATOR_PROPERTY)
    @NotNull(message = "La liste des connecteurs logiques ne doit pas être nulle")
    private List<String> countriesOperator;


}
