package fr.abes.periscope.web.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import fr.abes.periscope.core.criterion.CriterionCountry;
import fr.abes.periscope.core.criterion.CriterionTitleWords;
import lombok.Getter;
import lombok.Setter;

import javax.validation.constraints.NotNull;

/**
 * Représente un critère de recherche générique
 */
@Getter
@Setter
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "type")
@JsonSubTypes({@JsonSubTypes.Type(value = CriterionPcpWebDto.class, name = CriterionTypeName.CRITERION_PCP),
        @JsonSubTypes.Type(value = CriterionRcrWebDto.class, name = CriterionTypeName.CRITERION_RCR),
        @JsonSubTypes.Type(value = CriterionPpnWebDto.class, name = CriterionTypeName.CRITERION_PPN),
        @JsonSubTypes.Type(value = CriterionTitleWordsWebDto.class, name = CriterionTypeName.CRITERION_TITLE_WORDS),
        @JsonSubTypes.Type(value = CriterionCountryWebDto.class, name = CriterionTypeName.CRITERION_COUNTRIES)})
public abstract class CriterionWebDto {

    @JsonProperty(value="bloc_operator")
    @NotNull(message = "Le connecteur logique pour le bloc ne doit pas être null")
    protected String blocOperator;
}
