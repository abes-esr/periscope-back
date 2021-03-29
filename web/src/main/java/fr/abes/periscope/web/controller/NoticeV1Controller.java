package fr.abes.periscope.web.controller;

import fr.abes.periscope.core.criterion.Criterion;
import fr.abes.periscope.core.criterion.CriterionSort;
import fr.abes.periscope.core.entity.Notice;
import fr.abes.periscope.core.entity.v1.solr.NoticeV1SolrField;
import fr.abes.periscope.core.exception.IllegalCriterionException;
import fr.abes.periscope.core.service.NoticeStoreService;
import fr.abes.periscope.web.dto.CriterionSortWebDto;
import fr.abes.periscope.web.dto.CriterionWebDto;
import fr.abes.periscope.web.dto.NoticeWebDto;
import fr.abes.periscope.web.dto.RequestParameters;
import fr.abes.periscope.web.util.DtoMapper;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
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
@Data
public class NoticeV1Controller extends NoticeAbstractController {

    private final NoticeStoreService noticeStoreService;

    /** Service pour le mapping DTO */
    private final DtoMapper dtoMapper;


    @PostMapping("/notice/findByCriteria")
    public List<NoticeWebDto> findNoticesbyCriteria(@RequestParam int page, @RequestParam int size,@RequestBody @Valid RequestParameters requestParameters) throws IllegalCriterionException {
        LinkedList<CriterionWebDto> userCriteria = requestParameters.getUserCriteria();
        List<Criterion> criteria = findByCriteria(userCriteria, dtoMapper);
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
        return dtoMapper.mapList(candidate, NoticeWebDto.class);
    }
}
