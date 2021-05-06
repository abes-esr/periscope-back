package fr.abes.periscope.core.criterion;

import fr.abes.periscope.core.exception.IllegalOperatorException;
import lombok.Getter;

/**
 * Représente un bloc générique de recherche.
 * Une requête de recherche est formalisée selon :
 * Bloc0 <span class="strong">Connecteur1 Bloc1</span> Connecteur2 Bloc2 ConnecteurN BlocN
 * Cette classe représente un bloc générique de recherche avec son connecteur logique.
 */
@Getter
public abstract class Criterion {

    /** Vrai si le bloc est le premier bloc de la requête : Bloc0
     *  Faux si le bloc est connecté avec un autre bloc via @link{#blocOperator}
     * Connecteur1 Bloc1
     */
    protected Boolean isFirst;

    /** Connecteur logique pour le bloc (ET/OU/SAUF) */
    protected String blocOperator;

    /**
     * Constrcteur de critère de recherche pour un premier bloc.
     * L'opérateur appliqué par défaut est @link{LogicalOperator.AND}
     */
    protected Criterion() {
        this.isFirst = true;
        this.blocOperator = LogicalOperator.AND; //Par defaut
    }

    /**
     * Constrcteur de critère de recherche avec un connecteur de bloc
     * @param operator String Connecteur logique
     * @throws IllegalOperatorException si le connecteur n'est pas reconnu
     */
    protected Criterion(String operator) {

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
                throw new IllegalOperatorException("Bloc operator doesn't match ["+LogicalOperator.AND+", "+LogicalOperator.OR+", "+LogicalOperator.EXCEPT+"]");
        }
    }
}
