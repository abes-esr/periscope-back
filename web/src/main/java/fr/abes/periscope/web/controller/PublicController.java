package fr.abes.periscope.web.controller;

import fr.abes.periscope.core.criterion.*;
import fr.abes.periscope.core.entity.Notice;
import fr.abes.periscope.core.exception.IllegalCriterionException;
import fr.abes.periscope.core.exception.IllegalOperatorException;
import fr.abes.periscope.core.service.NoticeStoreService;
import fr.abes.periscope.web.dto.*;
import fr.abes.periscope.web.util.DtoMapper;
import fr.abes.periscope.core.util.TrackExecutionTime;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

@Slf4j
@CrossOrigin(origins = "*")
@RestController
@RequestMapping("/api/v1")
public class PublicController {

    private final NoticeStoreService noticeStoreService;

    /** Service pour le mapping DTO */
    @Autowired
    private DtoMapper dtoMapper;

    @Autowired
    public PublicController(NoticeStoreService service) {
        noticeStoreService = service;
    }

    @TrackExecutionTime
    @PostMapping("/notice/findByCriteria")
    public List<NoticeWebDto> findNoticesbyCriteria(@RequestParam int page, @RequestParam int size,@RequestBody @Valid LinkedList<CriterionWebDto> userCriteria) throws IllegalCriterionException {

        if (userCriteria.size() == 0) {
            throw new IllegalCriterionException("Criteria list cannot be empty");
        }
        List<Criterion> criteria = new LinkedList<>();

        Iterator<CriterionWebDto> criteriaIterator = userCriteria.iterator();
        while (criteriaIterator.hasNext()) {
            CriterionWebDto userCriterion = criteriaIterator.next();

            if (userCriterion instanceof CriterionPcpWebDto) {
                    criteria.add(dtoMapper.map(userCriterion, CriterionPcp.class));
            }

            if (userCriterion instanceof CriterionRcrWebDto) {
                criteria.add(dtoMapper.map(userCriterion, CriterionRcr.class));
            }

            if (userCriterion instanceof CriterionPpnWebDto) {
                criteria.add(dtoMapper.map(userCriterion, CriterionPpn.class));
            }

            if (userCriterion instanceof CriterionTitleWordsWebDto) {
                criteria.add(dtoMapper.map(userCriterion, CriterionTitleWords.class));
            }

            if (userCriterion instanceof CriterionCountryWebDto) {
                criteria.add(dtoMapper.map(userCriterion, CriterionCountry.class));
            }

            if (userCriterion instanceof CriterionEditorWebDto) {
                criteria.add(dtoMapper.map(userCriterion, CriterionEditor.class));
            }

            if (userCriterion instanceof CriterionIssnWebDto) {
                criteria.add(dtoMapper.map(userCriterion, CriterionIssn.class));
            }
        }

        List<Notice> candidate = noticeStoreService.findNoticesByCriteria(criteria,page,size);

        log.debug("List size is "+candidate.size());

        return dtoMapper.mapList(candidate, NoticeWebDto.class);

    }
}
