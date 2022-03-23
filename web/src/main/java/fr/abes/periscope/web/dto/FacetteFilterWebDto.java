package fr.abes.periscope.web.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Getter;
import lombok.Setter;

import javax.validation.constraints.NotNull;

@Getter
@Setter
public class FacetteFilterWebDto {
    public static final String ZONE = "zone";
    public static final String VALEUR = "valeur";

    @NotNull(message = "La zone est obligatoire")
    @JsonProperty(value = ZONE)
    private String zone;

    @NotNull(message = "La valeur de la facette est obligatoire")
    @JsonProperty(value = VALEUR)
    private String valeur;
}
