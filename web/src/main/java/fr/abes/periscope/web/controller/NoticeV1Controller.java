package fr.abes.periscope.web.controller;

import fr.abes.periscope.core.criterion.Criterion;
import fr.abes.periscope.core.criterion.CriterionSort;
import fr.abes.periscope.core.entity.Notice;
import fr.abes.periscope.core.entity.v1.solr.NoticeV1SolrField;
import fr.abes.periscope.core.service.NoticeStoreService;
import fr.abes.periscope.web.dto.NoticeWebV1Dto;
import fr.abes.periscope.web.dto.RequestParameters;
import fr.abes.periscope.web.dto.criterion.CriterionSortWebDto;
import fr.abes.periscope.web.dto.criterion.CriterionWebDto;
import fr.abes.periscope.web.util.DtoMapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
/**
 * @Deprecated : Controller d'interrogation de la V1 de l'index Solr
 */
@Deprecated
@Slf4j
@CrossOrigin(origins = "*")
@RestController
@RequestMapping("/api/v1")
public class NoticeV1Controller extends NoticeAbstractController {

    /**
     * Constructeur du contrôlleur pour les Notices V1
     * @param service Service de Notices
     * @param mapper Mapper Entité - DTO
     */
    @Autowired
    public NoticeV1Controller(NoticeStoreService service, DtoMapper mapper) {
       super(service,mapper);
    }

    /**
     * Rechercher des notices par des critères de recherche et des tris
     * Le critère de tris par défaut est par PPN en ordre croissant
     * @param page Numéro de la page (URL)
     * @param size Nombre d'élement (URL)
     * @param requestParameters Paramètre de la requêtes (body-content)
     * @return List<NoticeWebV1Dto> Liste de Notices V1 au format DTO
     */
    @PostMapping("/notice/findByCriteria")
    public List<NoticeWebV1Dto> findNoticesbyCriteria(@RequestParam int page, @RequestParam int size, @RequestBody @Valid RequestParameters requestParameters) {
        LinkedList<CriterionWebDto> userCriteria = requestParameters.getUserCriteria();
        List<Criterion> criteria = convertCriteriaFromDto(userCriteria);
        LinkedList<CriterionSortWebDto> userSortCriteria = requestParameters.getSortCriteria();

        List<CriterionSort> sortCriteria = new LinkedList<>();
        if (userSortCriteria != null && !userSortCriteria.isEmpty()) {
            Iterator<CriterionSortWebDto> userSortCriteriaIterator = userSortCriteria.iterator();
            while (userSortCriteriaIterator.hasNext()) {
                CriterionSortWebDto sortCriterion = userSortCriteriaIterator.next();
                sortCriteria.add(dtoMapper.map(sortCriterion, CriterionSort.class));
            }
        } else {
            sortCriteria.add(new CriterionSort(NoticeV1SolrField.PPN, Sort.Direction.ASC));
        }
        List<Notice> candidate = noticeStoreService.findNoticesByCriteria("v1", criteria,sortCriteria,page,size);
        return dtoMapper.mapList(candidate, NoticeWebV1Dto.class);
    }
}
