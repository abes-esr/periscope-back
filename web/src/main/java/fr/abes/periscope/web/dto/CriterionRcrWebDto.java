package fr.abes.periscope.web.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonTypeName;
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
@JsonTypeName(CriterionTypeName.CRITERION_RCR)
public class CriterionRcrWebDto extends CriterionWebDto {

    @JsonProperty(value="rcr")
    @NotNull(message = "La liste des codes RCR ne doit pas être nulle")
    private List<String> rcr = new ArrayList<>();

    @JsonProperty(value="rcr_operator")
    @NotNull(message = "La liste des connecteurs logiques ne doit pas être nulle")
    private List<String> rcrOperator = new ArrayList<>();

}
