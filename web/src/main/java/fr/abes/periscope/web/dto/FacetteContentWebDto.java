package fr.abes.periscope.web.dto;


import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class FacetteContentWebDto {
    @JsonProperty("key")
    private String key;
    @JsonProperty("occurrence")
    private Integer occurrence;

}
