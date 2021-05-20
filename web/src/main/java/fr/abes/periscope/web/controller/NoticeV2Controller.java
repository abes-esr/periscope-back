package fr.abes.periscope.web.controller;

import fr.abes.periscope.core.criterion.Criterion;
import fr.abes.periscope.core.criterion.CriterionSort;
import fr.abes.periscope.core.entity.Notice;
import fr.abes.periscope.core.entity.v2.solr.NoticeV2SolrField;
import fr.abes.periscope.core.entity.v2.solr.ResultSolr;
import fr.abes.periscope.core.service.NoticeStoreService;
import fr.abes.periscope.web.dto.FacetteWebDto;
import fr.abes.periscope.web.dto.NoticeWebV2Dto;
import fr.abes.periscope.web.dto.RequestParameters;
import fr.abes.periscope.web.dto.ResultWebDto;
import fr.abes.periscope.web.dto.criterion.CriterionFacetteWebDto;
import fr.abes.periscope.web.dto.criterion.CriterionSortWebDto;
import fr.abes.periscope.web.dto.criterion.CriterionWebDto;
import fr.abes.periscope.web.util.DtoMapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

/**
 *  Controller d'interrogation de la V2 de l'index Solr
 */
@Slf4j
@CrossOrigin(origins = "*")
@RestController
@RequestMapping("/api/v2")
public class NoticeV2Controller extends NoticeAbstractController {
    List<Criterion> criteria;
    List<CriterionSort> sortCriteria;
    List<String> facettes;
    /**
     * Constructeur du contrôlleur pour les Notices V1
     * @param service Service de Notices
     * @param mapper Mapper Entité - DTO
     */
    @Autowired
    public NoticeV2Controller(NoticeStoreService service, DtoMapper mapper) {
        super(service,mapper);
        this.criteria = new LinkedList<>();
        this.sortCriteria = new LinkedList<>();
        this.facettes = new ArrayList<>();
    }

    /**
     * Rechercher des notices par des critères de recherche et des tris.
     * Le critère de tris par défaut est par PPN en ordre croissant
     * @param page Numéro de la page (URL)
     * @param size Nombre d'élement (URL)
     * @param requestParameters Paramètre de la requêtes (body-content)
     * @return List<NoticeWebV2Dto> Liste de Notices V2 au format DTO
     */
    @PostMapping("/notice/findByCriteria")
    public List<NoticeWebV2Dto> findNoticesbyCriteria(@RequestParam int page, @RequestParam int size, @RequestBody @Valid RequestParameters requestParameters) {
        handleParameters(requestParameters);
        List<Notice> candidate = noticeStoreService.findNoticesByCriteria("v2", criteria,sortCriteria,page,size);
        return dtoMapper.mapList(candidate, NoticeWebV2Dto.class);
    }

    @PostMapping("/notice/findByCriteriaWithFacets")
    public ResultWebDto findNoticesByCriteriaWithFacets(@RequestParam int page, @RequestParam int size, @RequestBody @Valid RequestParameters requestParameters) {
        handleParameters(requestParameters);
        ResultSolr result = noticeStoreService.findNoticesWithFacets(this.criteria, this.facettes, this.sortCriteria, page, size);
        return dtoMapper.map(result, ResultWebDto.class);
    }

    private void handleParameters(RequestParameters requestParameters) {
        LinkedList<CriterionWebDto> userCriteria = requestParameters.getUserCriteria();
        this.criteria = convertCriteriaFromDto(userCriteria);

        LinkedList<CriterionSortWebDto> userSortCriteria = requestParameters.getSortCriteria();
        if (userSortCriteria != null && !userSortCriteria.isEmpty()) {
            Iterator<CriterionSortWebDto> userSortCriteriaIterator = userSortCriteria.iterator();
            while (userSortCriteriaIterator.hasNext()) {
                CriterionSortWebDto sortCriterion = userSortCriteriaIterator.next();
                sortCriterion.setVersion("v2"); /* Hack pour gérer les Notices V1 et V2 dans le NoticeMapper */
                this.sortCriteria.add(dtoMapper.map(sortCriterion, CriterionSort.class));
            }
        } else {
            this.sortCriteria.add(new CriterionSort(NoticeV2SolrField.PPN, Sort.Direction.ASC));
        }

        LinkedList<CriterionFacetteWebDto> facettesCriteria = requestParameters.getFacetCriteria();
        if (facettesCriteria != null && !facettesCriteria.isEmpty()) {
            Iterator<CriterionFacetteWebDto> facettesCriteriaIterator = facettesCriteria.iterator();
            while (facettesCriteriaIterator.hasNext()) {
                CriterionFacetteWebDto facetteCriterion = facettesCriteriaIterator.next();
                this.facettes.add(dtoMapper.map(facetteCriterion, String.class));
            }
        }
    }
}
