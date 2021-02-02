package fr.abes.periscope.core.exception;

/**
 * Si le critère n'est pas valide
 */
public class IllegalCriterionException extends IllegalArgumentException {

    public IllegalCriterionException(final String msg) {
        super(msg);
    }
}
