package fr.abes.periscope.web.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonTypeName;
import lombok.Getter;
import lombok.Setter;

import javax.validation.constraints.NotNull;
import java.util.ArrayList;
import java.util.List;

/**
 * Représente un critère de recherche par code Langue
 */
@Getter
@Setter
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
