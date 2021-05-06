package fr.abes.periscope.web.dto.criterion;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Getter;
import lombok.Setter;
import org.springframework.data.domain.Sort;

import javax.validation.constraints.NotNull;

/**
 * Représente un critère de tri au format JSON de l'API
 */
@Getter @Setter
public class CriterionSortWebDto {

    public static final String SORT_PROPERTY = "sort";
    public static final String ORDER_PROPERTY = "order";

    @JsonProperty(value = SORT_PROPERTY)
    @NotNull(message = "Le critère de tri ne peut pas être null")
    private String sort;

    @JsonProperty(value = ORDER_PROPERTY)
    @NotNull(message = "L'ordre de tri ne peut pas être null")
    private Sort.Direction order;

    /* Hack pour gérer les Notices V1 et V2 dans le NoticeMapper */
    @JsonIgnoreProperties
    private String version = "v1";
}
