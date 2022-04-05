package fr.abes.periscope.web.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Getter;
import lombok.Setter;

import javax.validation.constraints.NotNull;
import java.util.List;

@Getter
@Setter
public class FacetteFilterWebDto {
    public static final String ZONE = "zone";
    public static final String VALEURS = "valeurs";

    @NotNull(message = "La zone est obligatoire")
    @JsonProperty(value = ZONE)
    private String zone;

    @NotNull(message = "La valeur de la facette est obligatoire")
    @JsonProperty(value = VALEURS)
    private List<String> valeurs;
}
