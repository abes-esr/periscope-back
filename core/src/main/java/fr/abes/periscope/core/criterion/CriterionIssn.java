package fr.abes.periscope.core.criterion;

import fr.abes.periscope.core.exception.CriterionOperatorMismatchException;
import fr.abes.periscope.core.exception.IllegalOperatorException;
import lombok.Getter;

import java.util.ArrayList;
import java.util.List;

@Getter
public class CriterionIssn extends Criterion {

    /** Liste des ISSN à rechercher */
    private List<String> issn = new ArrayList<>();

    /** Liste des connecteurs logiques entre les ISSN
     * Exemple :
     * rcrOperator[0] pour connecter issn[0]
     * rcrOperator[1] pour connecter issn[0] et issn[1] */
    private List<String> issnOperator = new ArrayList<>();

    /**
     * Instancie un critère de recherche par ISSN à connecter avec un autre bloc
     * @param blocOperator Connecteur logique du bloc
     * @param candidatesIssn Liste des ISSN à rechercher
     * @param candidatesOperator Liste des connecteurs logiques entre les ISSN. Note: le premier critère n'a pas de connecteur.
     * @exception CriterionOperatorMismatchException Si le nombre de critères et le nombre d'opérateurs ne sont pas cohérent.
     * @exception IllegalOperatorException Si la liste de connecteurs contient des connecteurs inexistant ou interdit.
     */
    public CriterionIssn(String blocOperator, List<String> candidatesIssn, List<String> candidatesOperator) {
        super(blocOperator);

        if (candidatesOperator.size() != candidatesIssn.size()) {
            throw new CriterionOperatorMismatchException("Criteria list size mismatch the operators list size");
        }

        boolean onlyAcceptedOperator = candidatesOperator.stream().allMatch(operator -> (
                LogicalOperator.AND.equals(operator) ||
                        LogicalOperator.OR.equals(operator) ||
                        LogicalOperator.EXCEPT.equals(operator)));

        if (!onlyAcceptedOperator) {
            throw new IllegalOperatorException("Operators contains illegal values. Accepted value : "+LogicalOperator.AND+"/"+LogicalOperator.OR+"/"+LogicalOperator.EXCEPT);
        }

        this.issn = candidatesIssn;
        this.issnOperator = candidatesOperator;
    }

    /**
     * Instancie un critère de recherche par ISSN (1er bloc).
     * Le connecteur logique du bloc par défaut est ET
     * @param candidatesIssn Liste des ISSN à rechercher
     * @param candidatesOperator Liste des connecteurs logiques entre les ISSN. Note: le premier critère n'a pas de connecteur.
     * @exception CriterionOperatorMismatchException Si le nombre de critères et le nombre d'opérateurs ne sont pas cohérent.
     * @exception IllegalOperatorException Si la liste de connecteurs contient des connecteurs inexistant ou interdit.
     */
    public CriterionIssn(List<String> candidatesIssn, List<String> candidatesOperator) {
        super();

        if (candidatesOperator.size() != candidatesIssn.size()) {
            throw new CriterionOperatorMismatchException("Criteria list size mismatch the operators list size");
        }

        boolean onlyAcceptedOperator = candidatesOperator.stream().allMatch(operator -> (
                LogicalOperator.AND.equals(operator) ||
                        LogicalOperator.OR.equals(operator) ||
                        LogicalOperator.EXCEPT.equals(operator)));

        if (!onlyAcceptedOperator) {
            throw new IllegalOperatorException("Operators contains illegal values. Accepted value : "+LogicalOperator.AND+"/"+LogicalOperator.OR+"/"+LogicalOperator.EXCEPT);
        }

        this.issn = candidatesIssn;
        this.issnOperator = candidatesOperator;
    }
}
