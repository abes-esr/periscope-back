package fr.abes.periscope.web.controller;

import fr.abes.periscope.core.criterion.Criterion;
import fr.abes.periscope.core.criterion.CriterionFacette;
import fr.abes.periscope.core.criterion.CriterionSort;
import fr.abes.periscope.core.entity.solr.Notice;
import fr.abes.periscope.core.entity.solr.NoticeSolrField;
import fr.abes.periscope.core.entity.solr.ResultSolr;
import fr.abes.periscope.core.service.NoticeStoreService;
import fr.abes.periscope.core.util.UtilsMapper;
import fr.abes.periscope.web.dto.*;
import fr.abes.periscope.web.dto.criterion.CriterionFacetteWebDto;
import fr.abes.periscope.web.dto.criterion.CriterionSortWebDto;
import fr.abes.periscope.web.dto.criterion.CriterionWebDto;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
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
@Validated
public class NoticeController extends NoticeAbstractController {

    /**
     * Constructeur du contrôlleur pour les Notices V1
     * @param service Service de Notices
     * @param mapper Mapper Entité - DTO
     */
    @Autowired
    public NoticeController(NoticeStoreService service, UtilsMapper mapper) {
        super(service,mapper);
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
        List<Criterion> criteria = handleCriteria(requestParameters);
        List<CriterionSort> sortCriteria = handleSortCriteria(requestParameters);
        List<Notice> candidate = noticeStoreService.findNoticesByCriteria(criteria,sortCriteria,page,size);
        return mapper.mapList(candidate, NoticeWebV2Dto.class);
    }

    /**
     * Rechercher des notices par des critères de recherche et des tris, possibilité de rajouter des facettes
     * @param page numéro de la page (URL)
     * @param size nombre d'éléments (URL)
     * @param requestParameters paramètre de la requête (body-content)
     *                          Liste des critères de tris acceptés : title_type, ppn, issn, language, country, document_type, support_type, editor, title, start_year, end_year (case insensitive)
     * @return ResultWebDto : structure contenant la liste des notices, le nombre total de pages et les facettes
     */
    @PostMapping("/notice/findByCriteriaWithFacets")
    public ResultWebDto findNoticesByCriteriaWithFacets(@RequestParam int page, @RequestParam int size, @RequestBody @Valid RequestParameters requestParameters) {
        List<Criterion> criteria = handleCriteria(requestParameters);
        List<CriterionSort> sortCriteria = handleSortCriteria(requestParameters);
        List<String> facettes = handleFacettes(requestParameters);
        List<CriterionFacette> facettesFilter = handleFacettesFilters(requestParameters);
        ResultSolr result = noticeStoreService.findNoticesWithFacets(criteria, facettes, facettesFilter, sortCriteria, page, size);
        return mapper.map(result, ResultWebDto.class);
    }

    private List<Criterion> handleCriteria(RequestParameters requestParameters) {
        LinkedList<CriterionWebDto> userCriteria = requestParameters.getUserCriteria();
        return convertCriteriaFromDto(userCriteria);
    }

    private List<CriterionSort> handleSortCriteria(RequestParameters requestParameters) {
        List<CriterionSort> sortCriteria = new LinkedList<>();
        LinkedList<CriterionSortWebDto> userSortCriteria = requestParameters.getSortCriteria();
        if (userSortCriteria != null && !userSortCriteria.isEmpty()) {
            Iterator<CriterionSortWebDto> userSortCriteriaIterator = userSortCriteria.iterator();
            while (userSortCriteriaIterator.hasNext()) {
                CriterionSortWebDto sortCriterion = userSortCriteriaIterator.next();
                sortCriterion.setVersion("v2"); /* Hack pour gérer les Notices V1 et V2 dans le NoticeMapper */
                sortCriteria.add(mapper.map(sortCriterion, CriterionSort.class));
            }
        } else {
            sortCriteria.add(new CriterionSort(NoticeSolrField.PPN, Sort.Direction.ASC));
        }
        return sortCriteria;
    }

    private List<String> handleFacettes(RequestParameters requestParameters) {
        List<String> facettes = new LinkedList();
        LinkedList<CriterionFacetteWebDto> facettesCriteria = requestParameters.getFacetCriteria();
        if (facettesCriteria != null && !facettesCriteria.isEmpty()) {
            facettesCriteria.forEach(f -> facettes.add(f.getZone()));
        }
        return facettes;
    }

    private List<CriterionFacette> handleFacettesFilters(RequestParameters requestParameters) {
        List<CriterionFacette> facettesFilters = new LinkedList<>();
        LinkedList<FacetteFilterWebDto> facetteFilterCriteria = requestParameters.getFacetFilterCriteria();
        if (facetteFilterCriteria != null && !facetteFilterCriteria.isEmpty()) {
            facetteFilterCriteria.stream().forEach(f -> facettesFilters.add(new CriterionFacette(f.getZone(), f.getValeurs())));
        }
        return facettesFilters;
    }
}
