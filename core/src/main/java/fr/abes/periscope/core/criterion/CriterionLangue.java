package fr.abes.periscope.core.criterion;

import fr.abes.periscope.core.exception.CriterionOperatorMismatchException;
import fr.abes.periscope.core.exception.IllegalOperatorException;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

@Getter
public class CriterionLangue extends Criterion {

    /** Liste des codes Langue à rechercher */
    private List<String> langue = new ArrayList<>();

    /** Liste des connecteurs logiques entre les codes langues
     * Exemple :
     * rcrOperator[0] pour connecter langue[0]
     * rcrOperator[1] pour connecter langue[0] et langue[1] */
    private List<String> langueOperator = new ArrayList<>();

    /**
     * Instancie un critère de recherche par code langue à connecter avec un autre bloc
     * @param blocOperator Connecteur logique du bloc
     * @param candidatesLangue Liste des codes Langues à rechercher
     * @param candidatesOperator Liste des connecteurs logiques entre les code Langues. Note: le premier critère n'a pas de connecteur.
     * @exception CriterionOperatorMismatchException Si le nombre de critères et le nombre d'opérateurs ne sont pas cohérent.
     * @exception IllegalOperatorException Si la liste de connecteurs contient des connecteurs inexistant ou interdit.
     */
    public CriterionLangue(String blocOperator, List<String> candidatesLangue, List<String> candidatesOperator) {
        super(blocOperator);

        if (candidatesOperator.size() != candidatesLangue.size()) {
            throw new CriterionOperatorMismatchException("Criteria list size mismatch the operators list size");
        }

        boolean onlyAcceptedOperator = candidatesOperator.stream().allMatch(operator -> (
                LogicalOperator.AND.equals(operator) ||
                        LogicalOperator.OR.equals(operator) ||
                        LogicalOperator.EXCEPT.equals(operator)));

        if (!onlyAcceptedOperator) {
            throw new IllegalOperatorException("Operators contains illegal values. Accepted value : "+LogicalOperator.AND+"/"+LogicalOperator.OR+"/"+LogicalOperator.EXCEPT);
        }

        this.langue = candidatesLangue;
        this.langueOperator = candidatesOperator;
    }

    /**
     * Instancie un critère de recherche par code Langue (1er bloc).
     * Le connecteur logique du bloc par défaut est ET
     * @param candidatesLangue Liste des codes langue à rechercher
     * @param candidatesOperator Liste des connecteurs logiques entre les code langues. Note: le premier critère n'a pas de connecteur.
     * @exception CriterionOperatorMismatchException Si le nombre de critères et le nombre d'opérateurs ne sont pas cohérent.
     * @exception IllegalOperatorException Si la liste de connecteurs contient des connecteurs inexistant ou interdit.
     */
    public CriterionLangue(List<String> candidatesLangue, List<String> candidatesOperator) {
        super();

        if (candidatesOperator.size() != candidatesLangue.size()) {
            throw new CriterionOperatorMismatchException("Criteria list size mismatch the operators list size");
        }

        boolean onlyAcceptedOperator = candidatesOperator.stream().allMatch(operator -> (
                LogicalOperator.AND.equals(operator) ||
                        LogicalOperator.OR.equals(operator) ||
                        LogicalOperator.EXCEPT.equals(operator)));

        if (!onlyAcceptedOperator) {
            throw new IllegalOperatorException("Operators contains illegal values. Accepted value : "+LogicalOperator.AND+"/"+LogicalOperator.OR+"/"+LogicalOperator.EXCEPT);
        }

        this.langue = candidatesLangue;
        this.langueOperator = candidatesOperator;
    }
}
