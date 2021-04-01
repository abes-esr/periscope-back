package fr.abes.periscope.web.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonTypeName;
import lombok.Getter;
import lombok.Setter;

import javax.validation.constraints.NotNull;
import java.util.ArrayList;
import java.util.List;

/**
 * Représente un critère de recherche par mots du titre
 */
@Getter
@Setter
@JsonTypeName(CriterionTypeName.CRITERION_TITLE_WORDS)
public class CriterionTitleWordsWebDto extends CriterionWebDto {

    public static final String TITLE_WORDS_PROPERTY = "title_words";
    public static final String TITLE_WORDS_OPRATOR_PROPERTY = "title_words_operator";

    @JsonProperty(value= TITLE_WORDS_PROPERTY)
    @NotNull(message = "La liste des mots du titre ne doit pas être nulle")
    private List<String> titleWords;

    @JsonProperty(value= TITLE_WORDS_OPRATOR_PROPERTY)
    @NotNull(message = "La liste des connecteurs logiques ne doit pas être nulle")
    private List<String> titleWordsOperator;

}
