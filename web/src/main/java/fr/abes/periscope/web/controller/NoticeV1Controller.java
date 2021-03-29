package fr.abes.periscope.web.controller;

import fr.abes.periscope.core.exception.IllegalCriterionException;
import fr.abes.periscope.core.service.NoticeStoreService;
import fr.abes.periscope.web.dto.CriterionSortWebDto;
import fr.abes.periscope.web.dto.CriterionWebDto;
import fr.abes.periscope.web.dto.NoticeWebDto;
import fr.abes.periscope.web.dto.RequestParameters;
import fr.abes.periscope.web.util.DtoMapper;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
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
        LinkedList<CriterionSortWebDto> sortCriteria = requestParameters.getSortCriteria();
        return findByCriteria(page, size, noticeStoreService, userCriteria, sortCriteria, dtoMapper);
    }
}
