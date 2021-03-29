package fr.abes.periscope.core.criterion;

/**
 * Représente les valeurs que peut prendre un critère de tri (ascendant ou descendant)
 */
public final class CriterionSortName {
    public static final String SORTASCENDING = "asc";
    public static final String SORTDESCENDING = "desc";

    private CriterionSortName() {
        throw new IllegalStateException("Classe utilitaire");
    }
}
