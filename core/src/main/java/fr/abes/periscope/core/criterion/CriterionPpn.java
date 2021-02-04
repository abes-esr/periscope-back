package fr.abes.periscope.core.criterion;

import fr.abes.periscope.core.exception.IllegalCriterionException;
import fr.abes.periscope.core.exception.IllegalOperatorException;
import lombok.Getter;

import java.util.List;

@Getter
public class CriterionPpn extends Criterion {
    /** Liste des PPN à rechercher. Les connecteurs logiques entre les PPN sont forcément des OU */
    private List<String> ppn;

    /**
     * Instancie un critère de recherche par code RCR à connecter avec un autre bloc
     * @param blocOperator Connecteur logique du bloc
     * @param candidatesPpn Liste des PPN à rechercher
     * @exception IllegalOperatorException Si le connecteur du bloc est inexistant ou interdit
     * @exception IllegalCriterionException Si la liste des critères est vide
     */
    public CriterionPpn(String blocOperator, List<String> candidatesPpn) {
        super(blocOperator);

        if (candidatesPpn.isEmpty()) {
            throw new IllegalCriterionException("Criteria list cannot be empty");
        }

        this.ppn = candidatesPpn;
    }

    /**
     * Instancie un critère de recherche par PPN (1er bloc).
     * Le connecteur logique du bloc par défaut est ET
     * @param candidatesPpn Liste des PPN à rechercher
     * @exception IllegalOperatorException Si le connecteur du bloc est inexistant ou interdit
     * @exception IllegalCriterionException Si la liste des critères est vide
     */
    public CriterionPpn(List<String> candidatesPpn) {
        super();

        if (candidatesPpn.isEmpty()) {
            throw new IllegalCriterionException("Criteria list cannot be empty");
        }

        this.ppn = candidatesPpn;
    }


}
