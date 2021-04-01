package fr.abes.periscope.web.controller;

import fr.abes.periscope.core.criterion.*;
import fr.abes.periscope.core.exception.IllegalCriterionException;
import fr.abes.periscope.web.dto.*;
import fr.abes.periscope.web.util.DtoMapper;

import java.lang.reflect.Field;
import java.util.*;

public abstract class NoticeAbstractController {

    protected List<Criterion> findByCriteria(LinkedList<CriterionWebDto> userCriteria, DtoMapper dtoMapper) {
        if (userCriteria.isEmpty()) {
            throw new IllegalCriterionException("Json property '"+RequestParameters.CRITERIA_PROPERTY+"' is empty");
        }
        List<Criterion> criteria = new LinkedList<>();

        Iterator<CriterionWebDto> criteriaIterator = userCriteria.iterator();
        while (criteriaIterator.hasNext()) {
            CriterionWebDto userCriterion = criteriaIterator.next();
            handleCriteria(dtoMapper, criteria, userCriterion);
        }
        return criteria;
    }

    protected void handleCriteria(DtoMapper dtoMapper, List<Criterion> criteria, CriterionWebDto userCriterion) {

        if (userCriterion == null) {
            // Le type de critère n'a pas pu être décodé. On renvoie une erreur
            Class c = CriterionTypeName.class;
            List<String> type = new ArrayList<>();
            Arrays.stream(c.getDeclaredFields()).forEach(n -> {
                try {
                    Field f = c.getDeclaredField(n.getName());
                    type.add((String) f.get(n.getName()));
                } catch (IllegalAccessException|NoSuchFieldException ex) {
                    throw new IllegalCriterionException("Json property 'type' doesn't match any criterion type");
                }
            });
            throw new IllegalCriterionException("Json property 'type' doesn't match "+type.toString());
        }

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
