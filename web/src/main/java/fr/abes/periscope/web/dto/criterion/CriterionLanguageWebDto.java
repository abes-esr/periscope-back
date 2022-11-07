package fr.abes.periscope.web.dto.criterion;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonTypeName;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.validation.constraints.NotNull;
import java.util.List;

/**
 * Représente un critère de recherche par code Langue au format JSON de l'API
 */
@Getter @Setter
@NoArgsConstructor
@JsonTypeName(CriterionTypeName.CRITERION_LANGUAGE)
public class CriterionLanguageWebDto extends CriterionWebDto {

    public static final String LANGUAGE_PROPERTY = "language";
    public static final String LANGUAGE_OPERATOR_PROPERTY = "language_operator";

    @JsonProperty(value= LANGUAGE_PROPERTY)
    @NotNull(message = "La liste des codes langues ne doit pas être nulle")
    private List<String> language;

    @JsonProperty(value= LANGUAGE_OPERATOR_PROPERTY)
    @NotNull(message = "La liste des connecteurs logiques ne doit pas être nulle")
    private List<String> languageOperators;


}
