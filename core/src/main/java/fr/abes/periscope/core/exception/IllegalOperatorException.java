package fr.abes.periscope.core.exception;

/**
 * Si l'opérateur n'existe pas ou s'il n'est pas autorisé
 */
public class IllegalOperatorException extends IllegalArgumentException {

    public IllegalOperatorException(final String msg) {
        super(msg);
    }

}

