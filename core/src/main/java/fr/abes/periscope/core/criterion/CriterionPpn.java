package fr.abes.periscope.core.criterion;

import fr.abes.periscope.core.exception.CriterionOperatorMismatchException;
import fr.abes.periscope.core.exception.IllegalOperatorException;
import lombok.Getter;

import java.util.ArrayList;
import java.util.List;

@Getter
public class CriterionPpn extends Criterion {
    /** Liste des PPN à rechercher */
    private List<String> ppn;

    /** Liste des connecteurs logiques entre les PPN
     * Exemple :
     * rcrOperator[0] pour connecter ppn[0]
     * rcrOperator[1] pour connecter ppn[0] et ppn[1] */
    private List<String> ppnOperator;

    /**
     * Instancie un critère de recherche par code RCR à connecter avec un autre bloc
     * @param blocOperator Connecteur logique du bloc
     * @param candidatesPpn Liste des PPN à rechercher
     * @param candidatesOperator Liste des connecteurs logiques entre les PPN. Note: le premier critère n'a pas de connecteur.
     * @exception CriterionOperatorMismatchException Si le nombre de critères et le nombre d'opérateurs ne sont pas cohérent.
     * @exception IllegalOperatorException Si la liste de connecteurs contient des connecteurs inexistant ou interdit.
     */
    public CriterionPpn(String blocOperator, List<String> candidatesPpn, List<String> candidatesOperator) {
        super(blocOperator);

        if (candidatesOperator.size() != candidatesPpn.size()) {
            throw new CriterionOperatorMismatchException("Criteria list size mismatch the operators list size");
        }

        boolean onlyAcceptedOperator = candidatesOperator.stream().allMatch(operator -> (
                LogicalOperator.AND.equals(operator) ||
                        LogicalOperator.OR.equals(operator) ||
                        LogicalOperator.EXCEPT.equals(operator)));

        if (!onlyAcceptedOperator) {
            throw new IllegalOperatorException("Operators contains illegal values. Accepted value : "+LogicalOperator.AND+"/"+LogicalOperator.OR+"/"+LogicalOperator.EXCEPT);
        }

        this.ppn = candidatesPpn;
        this.ppnOperator = candidatesOperator;
    }

    /**
     * Instancie un critère de recherche par PPN (1er bloc).
     * Le connecteur logique du bloc par défaut est ET
     * @param candidatesPpn Liste des PPN à rechercher
     * @param candidatesOperator Liste des connecteurs logiques entre les PPN. Note: le premier critère n'a pas de connecteur.
     * @exception CriterionOperatorMismatchException Si le nombre de critères et le nombre d'opérateurs ne sont pas cohérent.
     * @exception IllegalOperatorException Si la liste de connecteurs contient des connecteurs inexistant ou interdit.
     */
    public CriterionPpn(List<String> candidatesPpn, List<String> candidatesOperator) {
        super();

        if (candidatesOperator.size() != candidatesPpn.size()) {
            throw new CriterionOperatorMismatchException("Criteria list size mismatch the operators list size");
        }

        boolean onlyAcceptedOperator = candidatesOperator.stream().allMatch(operator -> (
                LogicalOperator.AND.equals(operator) ||
                        LogicalOperator.OR.equals(operator) ||
                        LogicalOperator.EXCEPT.equals(operator)));

        if (!onlyAcceptedOperator) {
            throw new IllegalOperatorException("Operators contains illegal values. Accepted value : "+LogicalOperator.AND+"/"+LogicalOperator.OR+"/"+LogicalOperator.EXCEPT);
        }

        this.ppn = candidatesPpn;
        this.ppnOperator = candidatesOperator;
    }


}
