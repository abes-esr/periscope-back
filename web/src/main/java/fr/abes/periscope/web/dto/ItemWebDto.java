package fr.abes.periscope.web.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Getter;
import lombok.Setter;

/**
 * Représente un exemplaire de Notice au format JSON de l'API
 */
@Getter @Setter
public class ItemWebDto {

    @JsonProperty("epn")
    private String epn;

    @JsonProperty("rcr")
    private String rcr;

    @JsonProperty("pcp")
    private String pcp;

}
