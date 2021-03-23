package fr.abes.periscope.web.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonTypeName;
import fr.abes.periscope.core.criterion.CriterionTypeName;
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
    @JsonProperty(value="language")
    @NotNull(message = "La liste des codes langues ne doit pas être nulle")
    private List<String> language = new ArrayList<>();

    @JsonProperty(value="language_operator")
    @NotNull(message = "La liste des connecteurs logiques ne doit pas être nulle")
    private List<String> languageOperators = new ArrayList<>();
}
