package fr.abes.periscope.core.exception;

import java.util.InputMismatchException;

public class IllegalOperatorException extends InputMismatchException {

    public IllegalOperatorException(final String msg) {
        super(msg);
    }

}

