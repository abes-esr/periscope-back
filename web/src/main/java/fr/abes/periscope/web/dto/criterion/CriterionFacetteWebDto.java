package fr.abes.periscope.web.dto.criterion;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Getter;
import lombok.Setter;

import javax.validation.constraints.NotNull;

/**
 * Représente la liste des zones de facettes au format JSON de l'API
 */
@Getter
@Setter
public class CriterionFacetteWebDto {
    public static final String ZONE_PROPERTY = "zone";

    @JsonProperty(value = ZONE_PROPERTY)
    @NotNull(message = "La zone ne peut pas être null")
    private String zone;

}
