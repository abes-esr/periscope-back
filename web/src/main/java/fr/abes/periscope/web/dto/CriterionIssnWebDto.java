package fr.abes.periscope.web.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonTypeName;
import lombok.Getter;
import lombok.Setter;

import javax.validation.constraints.NotNull;
import java.util.ArrayList;
import java.util.List;

/**
 * Représente un critère de recherche par ISSN
 */
@Getter
@Setter
@JsonTypeName(CriterionTypeName.CRITERION_ISSN)
public class CriterionIssnWebDto extends CriterionWebDto {
    @JsonProperty(value="issn")
    @NotNull(message = "La liste des ISSN ne doit pas être nulle")
    private List<String> issn = new ArrayList<>();
}
