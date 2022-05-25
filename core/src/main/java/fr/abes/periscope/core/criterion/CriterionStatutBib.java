package fr.abes.periscope.core.criterion;

import fr.abes.periscope.core.exception.IllegalCriterionException;
import fr.abes.periscope.core.util.TYPE_NOTICE;
import lombok.Getter;

@Getter
public class CriterionStatutBib extends Criterion {
    private String statutBibliotheque;

    /**
     * Instancie un critère de recherche par code Statut de bibliothèque à connecter avec un autre bloc
     * @param blocOperator Connecteur logique du bloc
     * @param candidateStatut Statut de la bibliothèque à rechercher
     * @exception IllegalCriterionException Si la liste des critères est vide
     */
    public CriterionStatutBib(String blocOperator, String candidateStatut) {
        super(blocOperator, TYPE_NOTICE.BIBLIO);

        if (candidateStatut.equals("")) {
            throw new IllegalCriterionException("Criteria is empty");
        }

        this.statutBibliotheque = candidateStatut;
    }
}
