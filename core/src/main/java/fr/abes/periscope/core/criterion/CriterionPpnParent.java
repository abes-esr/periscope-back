package fr.abes.periscope.core.criterion;

import fr.abes.periscope.core.exception.IllegalCriterionException;
import fr.abes.periscope.core.exception.IllegalOperatorException;
import fr.abes.periscope.core.util.TYPE_NOTICE;
import lombok.Getter;

import java.util.List;

@Getter
public class CriterionPpnParent extends Criterion {

    /** Liste des PPN à rechercher.
     * Note : Les connecteurs logiques entre les PPN sont forcément des OU */
    private List<String> ppnParent;

    /**
     * Instancie un critère de recherche par code RCR à connecter avec un autre bloc
     * @param blocOperator Connecteur logique du bloc
     * @param candidatesPpn Liste des PPN à rechercher
     * @exception IllegalOperatorException Si le connecteur du bloc est inexistant ou interdit
     * @exception IllegalCriterionException Si la liste des critères est vide
     */
    public CriterionPpnParent(String blocOperator, List<String> candidatesPpn) {
        super(blocOperator, TYPE_NOTICE.EXEMPLAIRE);

        if (candidatesPpn.isEmpty()) {
            throw new IllegalCriterionException("Criteria list is empty");
        }

        this.ppnParent = candidatesPpn;
    }

    /**
     * Instancie un critère de recherche par PPN Parent (1er bloc).
     * Le connecteur logique du bloc par défaut est ET
     * @param candidatesPpn Liste des PPN à rechercher
     * @exception IllegalOperatorException Si le connecteur du bloc est inexistant ou interdit
     * @exception IllegalCriterionException Si la liste des critères est vide
     */
    public CriterionPpnParent(List<String> candidatesPpn) {
        super(TYPE_NOTICE.EXEMPLAIRE);

        if (candidatesPpn.isEmpty()) {
            throw new IllegalCriterionException("Criteria list is empty");
        }

        this.ppnParent = candidatesPpn;
    }


}
