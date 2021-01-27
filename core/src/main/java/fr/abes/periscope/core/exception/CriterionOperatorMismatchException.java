package fr.abes.periscope.core.exception;

import java.util.InputMismatchException;

/**
 * Exception si le nombre de critères et de connecteurs logiques ne sont pas cohérents
 */
public class CriterionOperatorMismatchException extends InputMismatchException {

    public CriterionOperatorMismatchException(final String msg) {
        super(msg);
    }
}

