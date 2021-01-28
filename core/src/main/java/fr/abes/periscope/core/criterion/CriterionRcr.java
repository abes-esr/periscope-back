package fr.abes.periscope.core.criterion;

import fr.abes.periscope.core.exception.CriterionOperatorMismatchException;
import fr.abes.periscope.core.exception.IllegalOperatorException;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.ArrayList;
import java.util.List;

/**
 * Représente un critère de recherche par code RCR
 */
@Getter
public class CriterionRcr extends Criterion {

    /** Liste des codes RCR à rechercher */
    private List<String> rcr = new ArrayList<>();

    /** Liste des connecteurs logiques entre les RCR
     * Exemple :
     * rcrOperator[0] pour connecter rcr[0]
     * rcrOperator[1] pour connecter rcr[0] et rcr[1] */
    private List<String> rcrOperator = new ArrayList<>();

    /**
     * Instancie un critère de recherche par code RCR
     * @param blocOperator Connecteur logique du bloc
     * @param candidatesRcr Liste des codes RCR à rechercher
     * @param candidatesOperator Liste des connecteurs logiques entre les code RCR. Note: le premier critère n'a pas de connecteur.
     * @exception CriterionOperatorMismatchException Si le nombre de critères et le nombre d'opérateurs ne sont pas cohérent.
     * @exception IllegalOperatorException Si la liste de connecteurs contient des connecteurs inexistant ou interdit.
     */
    public CriterionRcr(String blocOperator, List<String> candidatesRcr, List<String> candidatesOperator) {
        super(blocOperator);

        if (candidatesOperator.size() != candidatesRcr.size()) {
            throw new CriterionOperatorMismatchException("Criteria list size mismatch the operators list size");
        }

        boolean onlyAcceptedOperator = candidatesOperator.stream().allMatch(operator -> (
                LogicalOperator.AND.equals(operator) ||
                        LogicalOperator.OR.equals(operator) ||
                        LogicalOperator.EXCEPT.equals(operator)));

        if (!onlyAcceptedOperator) {
            throw new IllegalOperatorException("Operators contains illegal values. Accepted value : "+LogicalOperator.AND+"/"+LogicalOperator.OR+"/"+LogicalOperator.EXCEPT);
        }

        this.rcr = candidatesRcr;
        this.rcrOperator = candidatesOperator;
    }

    /**
     * Instancie un critère de recherche par code RCR.
     * Le connecteur logique du bloc par défaut est ET
     * @param candidatesRcr Liste des codes RCR à rechercher
     * @param candidatesOperator Liste des connecteurs logiques entre les code RCR. Note: le premier critère n'a pas de connecteur.
     * @exception CriterionOperatorMismatchException Si le nombre de critères et le nombre d'opérateurs ne sont pas cohérent.
     * @exception IllegalOperatorException Si la liste de connecteurs contient des connecteurs inexistant ou interdit.
     */
    public CriterionRcr(List<String> candidatesRcr, List<String> candidatesOperator) {
        super(LogicalOperator.AND);

        if (candidatesOperator.size() != candidatesRcr.size()) {
            throw new CriterionOperatorMismatchException("Criteria list size mismatch the operators list size");
        }

        boolean onlyAcceptedOperator = candidatesOperator.stream().allMatch(operator -> (
                LogicalOperator.AND.equals(operator) ||
                        LogicalOperator.OR.equals(operator) ||
                        LogicalOperator.EXCEPT.equals(operator)));

        if (!onlyAcceptedOperator) {
            throw new IllegalOperatorException("Operators contains illegal values. Accepted value : "+LogicalOperator.AND+"/"+LogicalOperator.OR+"/"+LogicalOperator.EXCEPT);
        }

        this.rcr = candidatesRcr;
        this.rcrOperator = candidatesOperator;
    }
}
