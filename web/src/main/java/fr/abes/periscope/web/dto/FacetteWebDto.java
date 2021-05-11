package fr.abes.periscope.web.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Getter;
import lombok.Setter;

import java.util.List;
import java.util.Map;

@Getter
@Setter
public class FacetteWebDto {
    @JsonProperty("zone")
    private String zone;
    @JsonProperty("valeurs")
    private List<Map<String, Integer>> valeurs;
}
