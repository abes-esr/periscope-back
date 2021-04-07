package fr.abes.periscope.web.dto.criterion;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonTypeName;
import lombok.Getter;
import lombok.Setter;

import javax.validation.constraints.NotNull;
import java.util.List;

/**
 * Représente un critère de recherche par code PCP au format JSON de l'API
 */
@Getter @Setter
@JsonTypeName(CriterionTypeName.CRITERION_PCP)
public class CriterionPcpWebDto extends CriterionWebDto {

    public static final String PCP_PROPERTY = "pcp";
    public static final String PCP_OPERATOR_PROPERTY = "pcp_operator";

    @JsonProperty(value=PCP_PROPERTY)
    @NotNull(message = "La liste des codes PCP ne doit pas être nulle")
    private List<String> pcp;

    @JsonProperty(value= PCP_OPERATOR_PROPERTY)
    @NotNull(message = "La liste des connecteurs logiques ne doit pas être nulle")
    private List<String> pcpOperator;

}
