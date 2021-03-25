package fr.abes.periscope.core.criterion;

import lombok.Getter;
import lombok.Setter;
import org.springframework.data.domain.Sort;

@Getter
@Setter
public class CriterionSort {
    private String sort;
    private Sort.Direction order;

    public CriterionSort(String sort, Sort.Direction order) {
        this.sort = sort;
        this.order = order;
    }
}
