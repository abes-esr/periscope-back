package fr.abes.periscope.core.criterion;

import fr.abes.periscope.core.exception.IllegalOperatorException;
import lombok.Getter;
import lombok.Setter;

/**
 * Représente un bloc générique de critère de recherche
 */
@Getter
public abstract class Criterion {

    /** Vrai si le bloc est le premier bloc de la requête
     *  Faux si le bloc est connecté avec un autre bloc via @link{#blocOperator} */
    protected Boolean isFirst;

    /** Connecteur logique pour le bloc (ET/OU/SAUF) */
    protected String blocOperator;

    /**
     * Constrcteur de critère de recherche pour un premier bloc
     */
    public Criterion() {
        this.isFirst = true;
        this.blocOperator = LogicalOperator.AND; //Par defaut
    }

    /**
     * Constrcteur de critère de recherche avec un connecteur de bloc
     * @param operator String Connecteur logique
     */
    public Criterion(String operator) {

        this.isFirst = false;

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
