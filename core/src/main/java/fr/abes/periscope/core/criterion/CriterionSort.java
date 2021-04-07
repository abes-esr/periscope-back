package fr.abes.periscope.core.criterion;

import lombok.Getter;
import lombok.Setter;
import org.springframework.data.domain.Sort;

/**
 * Représente un critère de tri
 */
@Getter
@Setter
public class CriterionSort {

    /** Nom du champs à trier */
    private String sort;
    /** Ordre du tri */
    private Sort.Direction order;

    /**
     * Constructeur d'un critère de tri
     * @param sort Nom du champs à trier
     * @param order Ordre du tri
     */
    public CriterionSort(String sort, Sort.Direction order) {
        this.sort = sort;
        this.order = order;
    }
}
