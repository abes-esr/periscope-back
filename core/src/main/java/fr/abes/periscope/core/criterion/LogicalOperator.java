package fr.abes.periscope.core.criterion;

/**
 * Représente les mots-clès des connecteurs logiques entre les critères de recherche
 */
public final class LogicalOperator {

    /** Connecteur logique ET */
    public static final String AND = "ET";
    /** Connecteur logique OU */
    public static final String OR = "OU";
    /** Connecteur logique SAUF */
    public static final String EXCEPT = "SAUF";

    private LogicalOperator() {
        throw new IllegalStateException("Classe utilitaire");
    }
}
