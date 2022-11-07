package fr.abes.periscope.core.exception;

/**
 * Si le critère de tri n'existe pas ou s'il n'est pas autorisé
 */
public class IllegalSortException extends IllegalArgumentException {

    public IllegalSortException(final String msg) {
        super(msg);
    }

}

