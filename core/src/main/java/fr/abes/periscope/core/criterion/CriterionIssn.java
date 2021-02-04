package fr.abes.periscope.core.criterion;

import fr.abes.periscope.core.exception.IllegalCriterionException;
import fr.abes.periscope.core.exception.IllegalOperatorException;
import lombok.Getter;

import java.util.List;

@Getter
public class CriterionIssn extends Criterion {

    /** Liste des ISSN à rechercher */
    private List<String> issn;

    /**
     * Instancie un critère de recherche par ISSN  avec un autre bloc
     * Le connecteur logique du bloc par défaut est OU
     * @param blocOperator Connecteur logique du bloc
     * @param candidatesIssn Liste des ISSN à rechercher
     * @exception IllegalOperatorException Si le connecteur du bloc est inexistant ou interdit
     * @exception IllegalCriterionException Si la liste des critères est vide
     */
    public CriterionIssn(String blocOperator, List<String> candidatesIssn) {
        super(blocOperator);

        if (candidatesIssn.isEmpty()) {
            throw new IllegalCriterionException("Criteria list cannot be empty");
        }

        this.issn = candidatesIssn;
    }

    /**
     * Instancie un critère de recherche par ISSN (1er bloc)
     * Le connecteur logique du bloc par défaut est ET
     * @param candidatesIssn Liste des ISSN à rechercher
     * @exception IllegalOperatorException Si le connecteur du bloc est inexistant ou interdit
     * @exception IllegalCriterionException Si la liste des critères est vide
     */
    public CriterionIssn(List<String> candidatesIssn) {
        super();

        if (candidatesIssn.isEmpty()) {
            throw new IllegalCriterionException("Criteria list cannot be empty");
        }

        this.issn = candidatesIssn;
    }
}
