package fr.abes.periscope.core.criterion;

import fr.abes.periscope.core.exception.CriterionOperatorMismatchException;
import fr.abes.periscope.core.exception.IllegalOperatorException;
import lombok.Getter;

import java.util.List;

@Getter
public class CriterionIssn extends Criterion {

    /** Liste des ISSN à rechercher */
    private List<String> issn;

    /**
     * Instancie un critère de recherche par ISSN (1er bloc).
     * Le connecteur logique du bloc par défaut est ET
     * @param blocOperator Connecteur logique du bloc
     * @param candidatesIssn Liste des ISSN à rechercher
     * @exception CriterionOperatorMismatchException Si le nombre de critères et le nombre d'opérateurs ne sont pas cohérent.
     * @exception IllegalOperatorException Si la liste de connecteurs contient des connecteurs inexistant ou interdit.
     */
    public CriterionIssn(String blocOperator, List<String> candidatesIssn) {
        super(blocOperator);
        this.issn = candidatesIssn;
    }

    /**
     * Instancie un critère de recherche par ISSN (1er bloc).
     * Le connecteur logique du bloc par défaut est ET
     * @param candidatesIssn Liste des ISSN à rechercher
     */
    public CriterionIssn(List<String> candidatesIssn) {
        super();
        this.issn = candidatesIssn;
    }
}
