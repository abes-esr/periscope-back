package fr.abes.periscope.web.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.util.LinkedList;

@Data
public class RequestParameters {
    @JsonProperty(value = "criteres")
    private LinkedList<CriterionWebDto> userCriteria;
    @JsonProperty(value = "tri")
    private LinkedList<CriterionSortWebDto> sortCriteria;
}
