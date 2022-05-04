package fr.abes.periscope.web.dto.criterion;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonTypeName;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.validation.constraints.NotNull;
import java.util.ArrayList;
import java.util.List;

/**
 * Représente un critère de recherche par code RCR au format JSON de l'API
 */
@Getter @Setter
@NoArgsConstructor
@JsonTypeName(CriterionTypeName.CRITERION_RCR)
public class CriterionRcrWebDto extends CriterionWebDto {

    public static final String RCR_PROPERTY = "rcr";
    public static final String RCR_OPERATOR_PROPERTY = "rcr_operator";

    @JsonProperty(value= RCR_PROPERTY)
    @NotNull(message = "La liste des codes RCR ne doit pas être nulle")
    private List<String> rcr;

    @JsonProperty(value= RCR_OPERATOR_PROPERTY)
    @NotNull(message = "La liste des connecteurs logiques ne doit pas être nulle")
    private List<String> rcrOperator;


    public CriterionRcrWebDto(String rcr, String rcrOperator, String blocOperator) {
        super(blocOperator);
        this.rcr = new ArrayList<>();
        this.rcr.add(rcr);
        this.rcrOperator = new ArrayList<>();
        this.rcrOperator.add(rcrOperator);
    }

}
