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
@JsonTypeName(CriterionTypeName.CRITERION_LANGUE)
public class CriterionLangueWebDto extends CriterionWebDto {
    @JsonProperty(value="langue")
    @NotNull(message = "La liste des codes langues ne doit pas être nulle")
    private List<String> langue = new ArrayList<>();

    @JsonProperty(value="langue_operator")
    @NotNull(message = "La liste des connecteurs logiques ne doit pas être nulle")
    private List<String> langueOperator = new ArrayList<>();
}
