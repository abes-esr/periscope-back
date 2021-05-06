package fr.abes.periscope.core.criterion;

import fr.abes.periscope.core.exception.CriterionOperatorMismatchException;
import fr.abes.periscope.core.exception.IllegalCriterionException;
import fr.abes.periscope.core.exception.IllegalOperatorException;
import lombok.Getter;

import java.util.ArrayList;
import java.util.List;

/**
 * Représente un critère de recherche par code pays
 */
@Getter
public class CriterionCountry extends Criterion {

    /** Liste des codes pays à rechercher */
    private List<String> countries = new ArrayList<>();

    /** Liste des connecteurs logiques entre les codes pays
     * Exemple :
     * countryOperator[0] pour connecter country[0]
     * countryOperator[1] pour connecter country[0] et country[1] */
    private List<String> countryOperators = new ArrayList<>();

    /**
     * Instancie un critère de recherche par code pays à connecter avec un autre bloc
     * @param blocOperator Connecteur logique du bloc
     * @param candidatesCountry Liste des codes pays à rechercher
     * @param candidatesOperator Liste des connecteurs logiques entre les code pays
     * @exception CriterionOperatorMismatchException Si le nombre de critères et le nombre d'opérateurs ne sont pas cohérent
     * @exception IllegalOperatorException Si la liste de connecteurs contient des connecteurs inexistant ou interdit
     * @exception IllegalCriterionException Si la liste des critères est vide
     */
    public CriterionCountry(String blocOperator, List<String> candidatesCountry, List<String> candidatesOperator) {
        super(blocOperator);

        if (candidatesCountry.isEmpty()) {
            throw new IllegalCriterionException("Criteria list is empty");
        }

        if (candidatesOperator.size() != candidatesCountry.size()) {
            throw new CriterionOperatorMismatchException("Criteria list size mismatch the operators list size");
        }

        boolean onlyAcceptedOperator = candidatesOperator.stream().allMatch(operator -> (
                LogicalOperator.AND.equals(operator) ||
                        LogicalOperator.OR.equals(operator) ||
                        LogicalOperator.EXCEPT.equals(operator)));

        if (!onlyAcceptedOperator) {
            throw new IllegalOperatorException("Operators contains illegal values. Accepted value : "+LogicalOperator.AND+"/"+LogicalOperator.OR+"/"+LogicalOperator.EXCEPT);
        }

        this.countries = candidatesCountry;
        this.countryOperators = candidatesOperator;
    }

    /**
     * Instancie un critère de recherche par code pays (1er bloc)
     * Le connecteur logique du bloc par défaut est ET
     * @param candidatesCountry Liste des codes pays à rechercher
     * @param candidatesOperator Liste des connecteurs logiques entre les code pays
     * @exception CriterionOperatorMismatchException Si le nombre de critères et le nombre d'opérateurs ne sont pas cohérent
     * @exception IllegalOperatorException Si la liste de connecteurs contient des connecteurs inexistant ou interdit
     * @exception IllegalCriterionException Si la liste des critères est vide
     */
    public CriterionCountry(List<String> candidatesCountry, List<String> candidatesOperator) {
        super();

        if (candidatesCountry.isEmpty()) {
            throw new IllegalCriterionException("Criteria list is empty");
        }

        if (candidatesOperator.size() != candidatesCountry.size()) {
            throw new CriterionOperatorMismatchException("Criteria list size mismatch the operators list size");
        }

        boolean onlyAcceptedOperator = candidatesOperator.stream().allMatch(operator -> (
                LogicalOperator.AND.equals(operator) ||
                        LogicalOperator.OR.equals(operator) ||
                        LogicalOperator.EXCEPT.equals(operator)));

        if (!onlyAcceptedOperator) {
            throw new IllegalOperatorException("Operators contains illegal values. Accepted value : "+LogicalOperator.AND+"/"+LogicalOperator.OR+"/"+LogicalOperator.EXCEPT);
        }

        this.countries = candidatesCountry;
        this.countryOperators = candidatesOperator;
    }
}
