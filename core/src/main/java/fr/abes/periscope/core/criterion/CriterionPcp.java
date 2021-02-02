package fr.abes.periscope.core.criterion;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.ArrayList;
import java.util.List;

/**
 * Représente un critère de recherche par code PCP
 */
@Getter
public class CriterionPcp extends Criterion {

    /** Liste des codes PCP à rechercher. Les connecteurs
     * logiques entre les codes sont des OU */
    private List<String> pcp = new ArrayList<>();

    /**
     * Constructeur de critère de recherche par code PCP à connecter avec un autre bloc
     * @param blocOperator Connecteur logique du bloc
     * @param candidatePcp Liste de code PCP
     */
    public CriterionPcp(String blocOperator, List<String> candidatePcp) {
        super(blocOperator);
        this.pcp = candidatePcp;
    }

    /**
     * Constructeur de critère de recherche par code PCP (1er bloc)
     * @param candidatePcp Liste de code PCP
     */
    public CriterionPcp(List<String> candidatePcp) {
        super();
        this.pcp = candidatePcp;
    }
}
