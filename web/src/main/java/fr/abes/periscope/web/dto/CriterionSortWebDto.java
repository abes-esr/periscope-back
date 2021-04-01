package fr.abes.periscope.web.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Getter;
import lombok.Setter;
import org.springframework.data.domain.Sort;

import javax.validation.constraints.NotNull;

@Getter
@Setter
public class CriterionSortWebDto {

    public static final String SORT_PROPERTY = "sort";
    public static final String ORDER_PROPERTY = "order";

    @JsonProperty(value = SORT_PROPERTY)
    @NotNull(message = "Le critère de tri ne peut pas être vide")
    private String sort;

    @JsonProperty(value = ORDER_PROPERTY)
    @NotNull(message = "L'ordre de tri ne peut pas être vide")
    private Sort.Direction order;
}
