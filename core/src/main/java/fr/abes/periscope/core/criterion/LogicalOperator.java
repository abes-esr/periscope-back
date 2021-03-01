package fr.abes.periscope.core.criterion;

/**
 * Représente les mots-clès des connecteurs logiques entre les critères de recherche
 */
public interface LogicalOperator {

    /** Connecteur logique ET */
    String AND = "ET";
    /** Connecteur logique OU */
    String OR = "OU";
    /** Connecteur logique SAUF */
    String EXCEPT = "SAUF";
}
