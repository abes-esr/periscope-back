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
 * Représente un critère de recherche par code RCR
 */
@Getter
@Setter
@JsonTypeName(CriterionTypeName.CRITERION_EDITOR)
public class CriterionEditorWebDto extends CriterionWebDto {

    @JsonProperty(value="editors")
    @NotNull(message = "La liste des codes RCR ne doit pas être nulle")
    private List<String> editors = new ArrayList<>();

    @JsonProperty(value="editors_operator")
    @NotNull(message = "La liste des connecteurs logiques ne doit pas être nulle")
    private List<String> editorsOperator = new ArrayList<>();

}
