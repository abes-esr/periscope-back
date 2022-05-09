package fr.abes.periscope.core.criterion;

import fr.abes.periscope.core.exception.IllegalOperatorException;
import fr.abes.periscope.core.util.TYPE_NOTICE;
import lombok.Getter;
import lombok.Setter;

/**
 * Représente un bloc générique de recherche.
 * Une requête de recherche est formalisée selon :
 * Bloc0 <span class="strong">Connecteur1 Bloc1</span> Connecteur2 Bloc2 ConnecteurN BlocN
 * Cette classe représente un bloc générique de recherche avec son connecteur logique.
 */
@Getter
@Setter
public abstract class Criterion {

    /** Vrai si le bloc est le premier bloc de la requête : Bloc0
     *  Faux si le bloc est connecté avec un autre bloc via @link{#blocOperator}
     * Connecteur1 Bloc1
     */
    protected Boolean isFirst;

    /** Connecteur logique pour le bloc (ET/OU/SAUF) */
    protected String blocOperator;

    protected TYPE_NOTICE typeNotice;
    /**
     * Constrcteur de critère de recherche pour un premier bloc.
     * L'opérateur appliqué par défaut est @link{LogicalOperator.AND}
     */
    protected Criterion() {
        this.isFirst = true;
        this.typeNotice = TYPE_NOTICE.BIBLIO;
        this.blocOperator = LogicalOperator.AND; //Par defaut
    }

    protected Criterion(TYPE_NOTICE typeNotice) {
        this.isFirst = true;
        this.typeNotice = typeNotice;
        this.blocOperator = LogicalOperator.AND;
    }

    /**
     * Constrcteur de critère de recherche avec un connecteur de bloc
     * @param operator String Connecteur logique
     * @param typeNotice Indique le type de notice concernée par le critère
     * @throws IllegalOperatorException si le connecteur n'est pas reconnu
     */
    protected Criterion(String operator, TYPE_NOTICE typeNotice) {
        this.isFirst = false;
        this.typeNotice = typeNotice;

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
