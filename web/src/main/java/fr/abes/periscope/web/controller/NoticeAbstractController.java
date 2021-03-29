package fr.abes.periscope.web.controller;

import fr.abes.periscope.core.criterion.*;
import fr.abes.periscope.core.entity.Notice;
import fr.abes.periscope.core.entity.v1.solr.NoticeV1SolrField;
import fr.abes.periscope.core.exception.IllegalCriterionException;
import fr.abes.periscope.core.service.NoticeStoreService;
import fr.abes.periscope.web.dto.*;
import fr.abes.periscope.web.util.DtoMapper;
import org.springframework.data.domain.Sort;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

public abstract class NoticeAbstractController {

    protected List<NoticeWebDto> findByCriteria(int page, int size, NoticeStoreService service, LinkedList<CriterionWebDto> userCriteria, LinkedList<CriterionSortWebDto> userSortCriteria, DtoMapper dtoMapper) {
        if (userCriteria.isEmpty()) {
            throw new IllegalCriterionException("Criteria list cannot be empty");
        }
        List<Criterion> criteria = new LinkedList<>();

        Iterator<CriterionWebDto> criteriaIterator = userCriteria.iterator();
        while (criteriaIterator.hasNext()) {
            CriterionWebDto userCriterion = criteriaIterator.next();
            handleCriteria(dtoMapper, criteria, userCriterion);
        }

        List<CriterionSort>  sortCriteria = new LinkedList<>();
        if (userSortCriteria != null && !userSortCriteria.isEmpty()) {
            Iterator<CriterionSortWebDto> userSortCriteriaIterator = userSortCriteria.iterator();
            while (userSortCriteriaIterator.hasNext()) {
                CriterionSortWebDto sortCriterion = userSortCriteriaIterator.next();
                sortCriteria.add(dtoMapper.map(sortCriterion, CriterionSort.class));
            }
        } else {
            sortCriteria.add(new CriterionSort(NoticeV1SolrField.PPN, Sort.Direction.ASC));
        }
        List<Notice> candidate = service.findNoticesByCriteria(criteria,sortCriteria,page,size);
        return dtoMapper.mapList(candidate, NoticeWebDto.class);
    }

    private void handleCriteria(DtoMapper dtoMapper, List<Criterion> criteria, CriterionWebDto userCriterion) {
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

        if (userCriterion instanceof CriterionLanguageWebDto) {
            criteria.add(dtoMapper.map(userCriterion, CriterionLanguage.class));
        }

        if (userCriterion instanceof CriterionEditorWebDto) {
            criteria.add(dtoMapper.map(userCriterion, CriterionEditor.class));
        }

        if (userCriterion instanceof CriterionIssnWebDto) {
            criteria.add(dtoMapper.map(userCriterion, CriterionIssn.class));
        }
    }
}
