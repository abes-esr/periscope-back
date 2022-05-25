package fr.abes.periscope.web.dto.criterion;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonTypeName;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.validation.constraints.NotNull;

@Getter @Setter
@NoArgsConstructor
@JsonTypeName(CriterionTypeName.CRITERION_PCP_RCR)
public class CriterionPcpRcrWebDto extends CriterionWebDto {
    public static final String PCP_PROPERTY = "pcp";
    public static final String RCR_PROPERTY = "rcr";

    @JsonProperty(value=PCP_PROPERTY)
    @NotNull(message = "Le code PCP ne doit pas être null")
    private String pcp;

    @JsonProperty(value= RCR_PROPERTY)
    @NotNull(message = "Le RCR ne doit pas être null")
    private String rcr;


    public CriterionPcpRcrWebDto(String pcp, String rcr, String blocOperator) {
        super(blocOperator);
        this.pcp = pcp;
        this.rcr = rcr;
    }
}
