package fr.abes.periscope.core.criterion;

import fr.abes.periscope.core.exception.CriterionOperatorMismatchException;
import fr.abes.periscope.core.exception.IllegalCriterionException;
import fr.abes.periscope.core.exception.IllegalOperatorException;
import fr.abes.periscope.core.util.TYPE_NOTICE;
import lombok.Getter;

import java.util.ArrayList;
import java.util.List;

@Getter
public class CriterionStatutBib extends Criterion {
    private List<String> statutBibliotheque;

    /** Liste des connecteurs logiques entre les statuts de bibliothèque
     * Exemple :
     * statutOperators[0] pour connecter statutBibliotheque[0]
     * statutOperators[1] pour connecter statutBibliotheque[0] et statutBibliotheque[1] */
    private List<String> statutOperators = new ArrayList<>();
    /**
     * Instancie un critère de recherche par code Statut de bibliothèque à connecter avec un autre bloc
     * @param blocOperator Connecteur logique du bloc
     * @param candidatesStatut Statut de la bibliothèque à rechercher
     * @Param candidatesOperator iste des connecteurs logiques entre les code langues
     * @exception CriterionOperatorMismatchException Si le nombre de critères et le nombre d'opérateurs ne sont pas cohérent
     * @exception IllegalCriterionException Si la liste des critères est vide
     */
    public CriterionStatutBib(String blocOperator, List<String> candidatesStatut, List<String> candidatesOperator) {
        super(blocOperator, TYPE_NOTICE.BIBLIO);

        if (candidatesStatut.isEmpty()) {
            throw new IllegalCriterionException("Criteria list is empty");
        }

        if (candidatesOperator.size() != candidatesStatut.size()) {
            throw new CriterionOperatorMismatchException("Criteria list size mismatch the operators list size");
        }

        boolean onlyAcceptedOperator = candidatesOperator.stream().allMatch(operator -> (
                LogicalOperator.AND.equals(operator) ||
                        LogicalOperator.OR.equals(operator) ||
                        LogicalOperator.EXCEPT.equals(operator)));

        if (!onlyAcceptedOperator) {
            throw new IllegalOperatorException("Operators contains illegal values. Accepted value : "+LogicalOperator.AND+"/"+LogicalOperator.OR+"/"+LogicalOperator.EXCEPT);
        }

        this.statutBibliotheque = candidatesStatut;
        this.statutOperators = candidatesOperator;
    }
}
