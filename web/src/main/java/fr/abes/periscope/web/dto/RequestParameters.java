package fr.abes.periscope.web.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import fr.abes.periscope.web.dto.criterion.CriterionSortWebDto;
import fr.abes.periscope.web.dto.criterion.CriterionWebDto;
import lombok.Data;

import javax.validation.constraints.NotNull;
import java.util.LinkedList;

/**
 * Représente une requête JSON pour recherche des Notices
 */
@Data
public class RequestParameters {

    public static final String CRITERIA_PROPERTY = "criteres";
    public static final String SORT_PROPERTY = "tri";

    @JsonProperty(value = CRITERIA_PROPERTY)
    @NotNull(message = "Json property "+CRITERIA_PROPERTY+" doesn't exists")
    private LinkedList<CriterionWebDto> userCriteria;

    @JsonProperty(value = SORT_PROPERTY)
    private LinkedList<CriterionSortWebDto> sortCriteria;
}
