package fr.abes.periscope.core.criterion;

import fr.abes.periscope.core.exception.IllegalOperatorException;
import lombok.Getter;
import lombok.Setter;

/**
 * Représente un bloc générique de critère de recherche
 */
@Getter
public abstract class Criterion {

    /** Connecteur logique pour le bloc (ET/OU/SAUF) */
    protected String blocOperator;

    public Criterion(String operator) {

        switch (operator) {
            case LogicalOperator.AND:
                this.blocOperator = LogicalOperator.AND;
                break;
            case LogicalOperator.OR:
                this.blocOperator = LogicalOperator.OR;
                break;
            case LogicalOperator.EXCEPT:
                this.blocOperator = LogicalOperator.EXCEPT;
                break;
            default:
                throw new IllegalOperatorException("Unable to decode '"+operator+"' as logical operator");
        }
    }
}
