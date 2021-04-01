package fr.abes.periscope.web.exception;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.exc.MismatchedInputException;

/**
 * Si le critère de tri n'existe pas ou s'il n'est pas autorisé
 */
public class MismatchedJsonTypeException extends Exception {

    public MismatchedJsonTypeException(final String msg) {
        super(msg);
    }

}

