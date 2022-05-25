package fr.abes.periscope.web.controller;

import fr.abes.periscope.core.criterion.*;
import fr.abes.periscope.core.exception.IllegalCriterionException;
import fr.abes.periscope.core.service.NoticeStoreService;
import fr.abes.periscope.core.util.TYPE_NOTICE;
import fr.abes.periscope.core.util.UtilsMapper;
import fr.abes.periscope.web.dto.RequestParameters;
import fr.abes.periscope.web.dto.criterion.*;

import java.lang.reflect.Field;
import java.util.*;

/**
 * Classe abstraite du contrôlleur pour les Notices
 */
public abstract class NoticeAbstractController {

    protected final NoticeStoreService noticeStoreService;

    /** Service pour le mapping Entité - DTO */
    protected final UtilsMapper mapper;

    /**
     * Constructeur d'un contrôlleur de Notice
     * @param service Service de Notice
     * @param mapper Mapper Entité - DTO
     */
    public NoticeAbstractController(NoticeStoreService service, UtilsMapper mapper) {
        this.noticeStoreService = service;
        this.mapper = mapper;
    }

    /**
     * Converti une liste de critères de recherche DTO en liste de critère de recherche
     * @param userCriteria Liste de critères de recherche DTO
     * @return List<Criterion> Liste de critères de recherche
     */
    protected List<Criterion> convertCriteriaFromDto(LinkedList<CriterionWebDto> userCriteria) {
        if (userCriteria.isEmpty()) {
            throw new IllegalCriterionException("Json property '"+ RequestParameters.CRITERIA_PROPERTY+"' is empty");
        }
        List<Criterion> criteria = new LinkedList<>();

        Iterator<CriterionWebDto> criteriaIterator = userCriteria.iterator();
        while (criteriaIterator.hasNext()) {
            CriterionWebDto userCriterion = criteriaIterator.next();
            handleCriteria(criteria, userCriterion);
        }
        return criteria;
    }

    /**
     * Converti le critère de recherche DTO en critère de recherche et l'ajout à la liste
     * @param criteria Liste de critère de recherche
     * @param userCriterion Critère de recherche DTO à convertir
     */
    private void handleCriteria(List<Criterion> criteria, CriterionWebDto userCriterion) {

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
            criteria.add(mapper.map(userCriterion, CriterionPcp.class));
        }

        if (userCriterion instanceof CriterionRcrWebDto) {
            criteria.add(mapper.map(userCriterion, CriterionRcr.class));
        }

        if (userCriterion instanceof CriterionPpnWebDto) {
            criteria.add(mapper.map(userCriterion, CriterionPpn.class));
        }

        if (userCriterion instanceof CriterionTitleWordsWebDto) {
            criteria.add(mapper.map(userCriterion, CriterionTitleWords.class));
        }

        if (userCriterion instanceof CriterionCountryWebDto) {
            criteria.add(mapper.map(userCriterion, CriterionCountry.class));
        }

        if (userCriterion instanceof CriterionLanguageWebDto) {
            criteria.add(mapper.map(userCriterion, CriterionLanguage.class));
        }

        if (userCriterion instanceof CriterionEditorWebDto) {
            criteria.add(mapper.map(userCriterion, CriterionEditor.class));
        }

        if (userCriterion instanceof CriterionIssnWebDto) {
            criteria.add(mapper.map(userCriterion, CriterionIssn.class));
        }

        if (userCriterion instanceof CriterionPcpRcrWebDto) {
            CriterionPcpWebDto critWebPcp = new CriterionPcpWebDto(((CriterionPcpRcrWebDto) userCriterion).getPcp(), LogicalOperator.AND, userCriterion.getBlocOperator());
            CriterionPcp critPcp = mapper.map(critWebPcp, CriterionPcp.class);
            critPcp.setTypeNotice(TYPE_NOTICE.EXEMPLAIRE);
            criteria.add(critPcp);

            CriterionRcrWebDto critWebRcr = new CriterionRcrWebDto(((CriterionPcpRcrWebDto) userCriterion).getRcr(), LogicalOperator.AND, userCriterion.getBlocOperator());
            CriterionRcr critRcr = mapper.map(critWebRcr, CriterionRcr.class);
            critRcr.setTypeNotice(TYPE_NOTICE.EXEMPLAIRE);
            criteria.add(critRcr);
        }
    }
}
