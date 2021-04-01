package fr.abes.periscope.web.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonTypeName;
import lombok.Getter;
import lombok.Setter;

import javax.validation.Valid;
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

    public static final String EDITORS_PROPERTY = "editors";
    public static final String EDITORS_OPERATOR_PROPERTY = "editors_operator";

    @JsonProperty(value= EDITORS_PROPERTY)
    @NotNull(message = "La liste des codes RCR ne doit pas être nulle")
    private List<String> editors;

    @JsonProperty(value= EDITORS_OPERATOR_PROPERTY)
    @NotNull(message = "La liste des connecteurs logiques ne doit pas être nulle")
    private List<String> editorsOperator;

}
