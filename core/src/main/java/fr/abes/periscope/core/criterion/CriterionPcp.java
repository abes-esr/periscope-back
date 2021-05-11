package fr.abes.periscope.core.criterion;

import fr.abes.periscope.core.exception.CriterionOperatorMismatchException;
import fr.abes.periscope.core.exception.IllegalCriterionException;
import fr.abes.periscope.core.exception.IllegalOperatorException;
import fr.abes.periscope.core.util.TYPE_NOTICE;
import lombok.Getter;

import java.util.ArrayList;
import java.util.List;

/**
 * Représente un critère de recherche par code PCP
 */
@Getter
public class CriterionPcp extends Criterion {

    /** Liste des codes PCP à rechercher. */
    private List<String> pcp = new ArrayList<>();

    /** Liste des connecteurs logiques entre les codes RCR
     * Exemple :
     * pcpOperators[0] pour connecter pcp[0]
     * pcpOperators[1] pour connecter pcp[0] et pcp[1] */
    private List<String> pcpOperators = new ArrayList<>();

    /**
     * Constructeur de critère de recherche par code PCP à connecter avec un autre bloc
     * @param blocOperator Connecteur logique du bloc
     * @param candidatesPcp Liste de code PCP
     * @param candidatesOperator Liste des connecteurs logiques entre les code PCP
     * @exception CriterionOperatorMismatchException Si le nombre de critères et le nombre d'opérateurs ne sont pas cohérent
     * @exception IllegalOperatorException Si la liste de connecteurs contient des connecteurs inexistant ou interdit
     * @exception IllegalCriterionException Si la liste des critères est vide
     */
    public CriterionPcp(String blocOperator, List<String> candidatesPcp, List<String> candidatesOperator) {
        super(blocOperator, TYPE_NOTICE.EXEMPLAIRE);

        if (candidatesPcp.isEmpty()) {
            throw new IllegalCriterionException("Criteria list is empty");
        }

        if (candidatesOperator.size() != candidatesPcp.size()) {
            throw new CriterionOperatorMismatchException("Criteria list size mismatch the operators list size");
        }

        boolean onlyAcceptedOperator = candidatesOperator.stream().allMatch(operator -> (
                LogicalOperator.AND.equals(operator) ||
                        LogicalOperator.OR.equals(operator) ||
                        LogicalOperator.EXCEPT.equals(operator)));

        if (!onlyAcceptedOperator) {
            throw new IllegalOperatorException("Operators contains illegal values. Accepted value : "+LogicalOperator.AND+"/"+LogicalOperator.OR+"/"+LogicalOperator.EXCEPT);
        }

        this.pcp = candidatesPcp;
        this.pcpOperators = candidatesOperator;
    }

    /**
     * Constructeur de critère de recherche par code PCP (1er bloc)
     * @param candidatesPcp Liste de code PCP
     * @param candidatesOperator Liste des connecteurs logiques entre les codes PCP
     * @exception CriterionOperatorMismatchException Si le nombre de critères et le nombre d'opérateurs ne sont pas cohérent
     * @exception IllegalOperatorException Si la liste de connecteurs contient des connecteurs inexistant ou interdit
     * @exception IllegalCriterionException Si la liste des critères est vide
     */
    public CriterionPcp(List<String> candidatesPcp, List<String> candidatesOperator) {
        super();

        if (candidatesPcp.isEmpty()) {
            throw new IllegalCriterionException("Criteria list is empty");
        }

        if (candidatesOperator.size() != candidatesPcp.size()) {
            throw new CriterionOperatorMismatchException("Criteria list size mismatch the operators list size");
        }

        boolean onlyAcceptedOperator = candidatesOperator.stream().allMatch(operator -> (
                LogicalOperator.AND.equals(operator) ||
                        LogicalOperator.OR.equals(operator) ||
                        LogicalOperator.EXCEPT.equals(operator)));

        if (!onlyAcceptedOperator) {
            throw new IllegalOperatorException("Operators contains illegal values. Accepted value : "+LogicalOperator.AND+"/"+LogicalOperator.OR+"/"+LogicalOperator.EXCEPT);
        }

        this.pcp = candidatesPcp;
        this.pcpOperators = candidatesOperator;
    }
}
