package fr.abes.periscope.core.criterion;

/**
 * Représente les noms des types de critères de recherche
 * Ces noms sont utilisés pour définir le type de critère (champs 'type" dans le JSON)
 */
public interface CriterionTypeName {

    String CRITERION_PCP = "CriterionPcp";
    String CRITERION_RCR = "CriterionRcr";
    String CRITERION_PPN = "CriterionPpn";
    String CRITERION_TITLE_WORDS = "CriterionTitleWords";
    String CRITERION_COUNTRIES = "CriterionCountry";
    String CRITERION_LANGUAGE = "CriterionLanguage";
    String CRITERION_EDITOR = "CriterionEditor";
    String CRITERION_ISSN = "CriterionIssn";
}