package fr.abes.periscope.core.repository.solr.impl;

import fr.abes.periscope.core.criterion.Criterion;
import fr.abes.periscope.core.criterion.CriterionPcp;
import fr.abes.periscope.core.criterion.CriterionRcr;
import fr.abes.periscope.core.criterion.LogicalOperator;
import fr.abes.periscope.core.entity.NoticeSolr;
import fr.abes.periscope.core.repository.solr.AdvancedNoticeRepository;
import fr.abes.periscope.core.repository.solr.NoticeField;
import fr.abes.periscope.core.util.TrackExecutionTime;
import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.solr.core.DefaultQueryParser;
import org.springframework.data.solr.core.SolrTemplate;
import org.springframework.data.solr.core.query.Criteria;
import org.springframework.data.solr.core.query.FilterQuery;
import org.springframework.data.solr.core.query.SimpleFilterQuery;
import org.springframework.data.solr.core.query.SimpleQuery;

import java.util.Iterator;
import java.util.List;

/**
 * Représente un dépôt de Notice SolR avec des requêtes complexes
 */
@AllArgsConstructor
@Slf4j
public class AdvancedNoticeRepositoryImpl implements AdvancedNoticeRepository {

    private SolrTemplate solrTemplate;

    /**
     * Construit la requête SolR à partir d'un critère de recherche par PCP
     * @param pcp Les critères de recherche par PCP
     * @return Criteria Requête SolR
     */
    private Criteria buildPcpQuery(CriterionPcp pcp) {

        if (pcp.getPcp().size() > 0) {

            Iterator<String> pcpIterator = pcp.getPcp().iterator();
            String pcpCode = pcpIterator.next();

            Criteria myCriteria = new Criteria(NoticeField.PCP).is(pcpCode);

            while (pcpIterator.hasNext()) {
                pcpCode = pcpIterator.next();
                myCriteria = myCriteria.or(NoticeField.PCP).is(pcpCode);
            }

            switch (pcp.getBlocOperator()) {
                case LogicalOperator.AND:
                    // AND par défaut, on ne fait rien
                    break;
                case LogicalOperator.OR:
                    myCriteria.setPartIsOr(true);
                    break;
                case LogicalOperator.EXCEPT:
                    myCriteria = myCriteria.notOperator();
                    break;
            }

            return myCriteria;

        } else {
            return null;
        }
    }

    /**
     * Construit la requête SolR à partir d'un critère de recherche par RCR
     * @param rcr Les critères de recherche par RCR
     * @return Criteria Requête SolR
     */
    private Criteria buildRcrQuery(CriterionRcr rcr) {

        if (rcr.getRcr().size() > 0) {

            Iterator<String> rcrIterator = rcr.getRcr().iterator();
            Iterator<String> rcrOperatorIterator = rcr.getRcrOperator().iterator();

            String rcrCode = rcrIterator.next();

            Criteria myCriteria = new Criteria(NoticeField.RCR).is(rcrCode).connect();

            while (rcrIterator.hasNext()) {
                rcrCode = rcrIterator.next();
                String rcrOperator = rcrOperatorIterator.next();

                switch (rcrOperator) {
                    case LogicalOperator.AND:
                        myCriteria = myCriteria.connect().and(NoticeField.RCR).is(rcrCode);
                        break;
                    case LogicalOperator.OR:
                        myCriteria = myCriteria.connect().or(NoticeField.RCR).is(rcrCode);
                        break;
                    case LogicalOperator.EXCEPT:
                        myCriteria = myCriteria.connect().and(NoticeField.RCR).is(rcrCode).not();
                        break;
                }
            }

            switch (rcr.getBlocOperator()) {
                case LogicalOperator.AND:
                    // AND par défaut, on ne fait rien
                    break;
                case LogicalOperator.OR:
                    myCriteria.setPartIsOr(true);
                    break;
                case LogicalOperator.EXCEPT:
                    myCriteria = myCriteria.notOperator();
                    break;
            }

            return myCriteria;

        } else {
            return null;
        }
    }

    @TrackExecutionTime
    @Override
    /**
     * Retourne les Notices SolR selon une liste de critères et une page
     * @param criteria Les critères de la recherche
     * @param page La page souhaitée
     * @return List<NoticeSolr> Liste de Notices SolR
     */
    public List<NoticeSolr> findNoticesByCriteria(List<Criterion> criteria, Pageable page) {

        FilterQuery filterQuery = new SimpleFilterQuery();

        Iterator<Criterion> criteriaIterator = criteria.iterator();
        while (criteriaIterator.hasNext()) {
            Criterion criterion = criteriaIterator.next();

            // Bloc de critère PCP
            if (criterion instanceof CriterionPcp) {

                Criteria pcpQuery = buildPcpQuery((CriterionPcp)criterion);
                if (pcpQuery != null) {
                    filterQuery.addCriteria(pcpQuery);
                }
            }

            // Bloc de critère RCR
            if (criterion instanceof CriterionRcr) {

                Criteria rcrQuery = buildRcrQuery((CriterionRcr)criterion);
                if (rcrQuery != null) {
                    filterQuery.addCriteria(rcrQuery);
                }
            }
        }

        SimpleQuery solrQuery = new SimpleQuery(filterQuery.getCriteria(),page);
        solrQuery.addProjectionOnFields(
                NoticeField.PPN,
                NoticeField.ISSN_T,
                NoticeField.PCP,
                NoticeField.RCR,
                NoticeField.EDITOR,
                NoticeField.PROCESSING_GLOBAL_DATA,
                NoticeField.KEY_TITLE,
                NoticeField.ISSN,
                NoticeField.KEY_SHORTED_TITLE,
                NoticeField.PROPER_TITLE,
                NoticeField.TITLE_FROM_DIFFERENT_AUTHOR,
                NoticeField.PARALLEL_TITLE,
                NoticeField.TITLE_COMPLEMENT,
                NoticeField.SECTION_TITLE,
                NoticeField.KEY_TITLE_QUALIFIER,
                NoticeField.CONTINIOUS_TYPE);

        // Debug query
        DefaultQueryParser dqp = new DefaultQueryParser(null);
        String actualQuery = dqp.getQueryString(solrQuery, null);
        log.debug("SolR query : "+actualQuery);

        Page results = solrTemplate.queryForPage("notice",solrQuery, NoticeSolr.class);
        return results.getContent();
    }
}
