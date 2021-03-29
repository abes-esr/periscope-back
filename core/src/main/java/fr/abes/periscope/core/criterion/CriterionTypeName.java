package fr.abes.periscope.core.criterion;

/**
 * Représente les noms des types de critères de recherche
 * Ces noms sont utilisés pour définir le type de critère (champs 'type" dans le JSON)
 */
public final class CriterionTypeName {

    public static final String CRITERION_PCP = "CriterionPcp";
    public static final String CRITERION_RCR = "CriterionRcr";
    public static final String CRITERION_PPN = "CriterionPpn";
    public static final String CRITERION_TITLE_WORDS = "CriterionTitleWords";
    public static final String CRITERION_COUNTRIES = "CriterionCountry";
    public static final String CRITERION_LANGUAGE = "CriterionLanguage";
    public static final String CRITERION_EDITOR = "CriterionEditor";
    public static final String CRITERION_ISSN = "CriterionIssn";

    private CriterionTypeName() {
        throw new IllegalStateException("Classe utilitaire");
    }
}
