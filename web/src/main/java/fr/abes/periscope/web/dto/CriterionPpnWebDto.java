package fr.abes.periscope.web.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonTypeName;
import lombok.Getter;
import lombok.Setter;

import javax.validation.constraints.NotNull;
import java.util.ArrayList;
import java.util.List;

/**
 * Représente un critère de recherche par PPN
 */
@Getter
@Setter
@JsonTypeName(CriterionTypeName.CRITERION_PPN)
public class CriterionPpnWebDto extends CriterionWebDto {

    public static final String PPN_PROPERTY = "ppn";
    public static final String PPN_OPERATOR_PROPERTY = "ppn_operator";

    @JsonProperty(value= PPN_PROPERTY)
    @NotNull(message = "La liste des PPN ne doit pas être nulle")
    private List<String> ppn;
}
