package fr.abes.periscope.core.criterion;

import fr.abes.periscope.core.exception.CriterionOperatorMismatchException;
import fr.abes.periscope.core.exception.IllegalCriterionException;
import fr.abes.periscope.core.exception.IllegalOperatorException;
import lombok.Getter;

import java.util.ArrayList;
import java.util.List;

/**
 * Représente un critère de recherche par code de langue
 */
@Getter
public class CriterionLanguage extends Criterion {

    /** Liste des codes Langue à rechercher */
    private List<String> languages = new ArrayList<>();

    /** Liste des connecteurs logiques entre les codes langues
     * Exemple :
     * languageOperators[0] pour connecter languages[0]
     * languageOperators[1] pour connecter languages[0] et languages[1] */
    private List<String> languageOperators = new ArrayList<>();

    /**
     * Instancie un critère de recherche par code langue à connecter avec un autre bloc
     * @param blocOperator Connecteur logique du bloc
     * @param candidatesLanguage Liste des codes Langues à rechercher
     * @param candidatesOperator Liste des connecteurs logiques entre les code langues
     * @exception CriterionOperatorMismatchException Si le nombre de critères et le nombre d'opérateurs ne sont pas cohérent
     * @exception IllegalOperatorException Si la liste de connecteurs contient des connecteurs inexistant ou interdit
     */
    public CriterionLanguage(String blocOperator, List<String> candidatesLanguage, List<String> candidatesOperator) {
        super(blocOperator);

        if (candidatesLanguage.isEmpty()) {
            throw new IllegalCriterionException("Criteria list is empty");
        }

        if (candidatesOperator.size() != candidatesLanguage.size()) {
            throw new CriterionOperatorMismatchException("Criteria list size mismatch the operators list size");
        }

        boolean onlyAcceptedOperator = candidatesOperator.stream().allMatch(operator -> (
                LogicalOperator.AND.equals(operator) ||
                        LogicalOperator.OR.equals(operator) ||
                        LogicalOperator.EXCEPT.equals(operator)));

        if (!onlyAcceptedOperator) {
            throw new IllegalOperatorException("Operators contains illegal values. Accepted value : "+LogicalOperator.AND+"/"+LogicalOperator.OR+"/"+LogicalOperator.EXCEPT);
        }

        this.languages = candidatesLanguage;
        this.languageOperators = candidatesOperator;
    }

    /**
     * Instancie un critère de recherche par code Langue (1er bloc).
     * Le connecteur logique du bloc par défaut est ET
     * @param candidatesLanguage Liste des codes langue à rechercher
     * @param candidatesOperator Liste des connecteurs logiques entre les code langues
     * @exception CriterionOperatorMismatchException Si le nombre de critères et le nombre d'opérateurs ne sont pas cohérent
     * @exception IllegalOperatorException Si la liste de connecteurs contient des connecteurs inexistant ou interdit
     */
    public CriterionLanguage(List<String> candidatesLanguage, List<String> candidatesOperator) {
        super();

        if (candidatesLanguage.isEmpty()) {
            throw new IllegalCriterionException("Criteria list is empty");
        }

        if (candidatesOperator.size() != candidatesLanguage.size()) {
            throw new CriterionOperatorMismatchException("Criteria list size mismatch the operators list size");
        }

        boolean onlyAcceptedOperator = candidatesOperator.stream().allMatch(operator -> (
                LogicalOperator.AND.equals(operator) ||
                        LogicalOperator.OR.equals(operator) ||
                        LogicalOperator.EXCEPT.equals(operator)));

        if (!onlyAcceptedOperator) {
            throw new IllegalOperatorException("Operators contains illegal values. Accepted value : "+LogicalOperator.AND+"/"+LogicalOperator.OR+"/"+LogicalOperator.EXCEPT);
        }

        this.languages = candidatesLanguage;
        this.languageOperators = candidatesOperator;
    }
}
