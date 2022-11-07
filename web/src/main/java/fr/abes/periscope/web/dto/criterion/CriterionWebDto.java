package fr.abes.periscope.web.dto.criterion;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import lombok.Data;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.validation.constraints.NotNull;

/**
 * Représente un critère de recherche générique au format JSON de l'API
 */
@NoArgsConstructor
@Getter
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "type")
@JsonSubTypes({@JsonSubTypes.Type(value = CriterionPcpWebDto.class, name = CriterionTypeName.CRITERION_PCP),
        @JsonSubTypes.Type(value = CriterionRcrWebDto.class, name = CriterionTypeName.CRITERION_RCR),
        @JsonSubTypes.Type(value = CriterionPpnWebDto.class, name = CriterionTypeName.CRITERION_PPN),
        @JsonSubTypes.Type(value = CriterionTitleWordsWebDto.class, name = CriterionTypeName.CRITERION_TITLE_WORDS),
        @JsonSubTypes.Type(value = CriterionCountryWebDto.class, name = CriterionTypeName.CRITERION_COUNTRIES),
        @JsonSubTypes.Type(value = CriterionLanguageWebDto.class, name = CriterionTypeName.CRITERION_LANGUAGE),
        @JsonSubTypes.Type(value = CriterionEditorWebDto.class, name = CriterionTypeName.CRITERION_EDITOR),
        @JsonSubTypes.Type(value = CriterionIssnWebDto.class, name = CriterionTypeName.CRITERION_ISSN),
        @JsonSubTypes.Type(value = CriterionPcpRcrWebDto.class, name = CriterionTypeName.CRITERION_PCP_RCR),
        @JsonSubTypes.Type(value = CriterionStatutBibWebDto.class, name = CriterionTypeName.CRITERION_STATUT_BIB)})
public abstract class CriterionWebDto {

    public static final String OPERATOR_PROPERTY = "bloc_operator";

    @JsonProperty(value=OPERATOR_PROPERTY)
    @NotNull(message = "Le connecteur logique pour le bloc ne doit pas être null")
    protected String blocOperator;

    public CriterionWebDto(String blocOperator) {
        this.blocOperator = blocOperator;
    }

}
