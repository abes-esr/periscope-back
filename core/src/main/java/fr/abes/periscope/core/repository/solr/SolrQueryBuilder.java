package fr.abes.periscope.core.repository.solr;

import fr.abes.periscope.core.criterion.*;
import fr.abes.periscope.core.exception.IllegalCriterionException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.solr.core.query.Criteria;
import org.springframework.data.solr.core.query.FilterQuery;
import org.springframework.data.solr.core.query.SimpleFilterQuery;

import java.util.Iterator;
import java.util.List;

/**
 * Représente un constructeur de requête SolR pour Periscope
 */
@Slf4j
public class SolrQueryBuilder {

    /**
     * Construit la requête SolR à partir des critères de recherche
     * @param criteria Critères de recherche
     * @return Criteria Requête SolR
     */
    public Criteria buildQuery(List<Criterion> criteria) {
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

            // Bloc de critère Mots du titre
            if (criterion instanceof CriterionTitleWords) {

                try {
                    Criteria titleWordsQuery = buildTitleWordsQuery((CriterionTitleWords) criterion);
                    filterQuery.addCriteria(titleWordsQuery);
                } catch(IllegalCriterionException ex) {
                    log.error(ex.getLocalizedMessage());
                }
            }
        }

        return filterQuery.getCriteria();
    }

    /**
     * Construit la requête SolR à partir d'un critère de recherche par PCP
     * @param pcp Les critères de recherche par PCP
     * @return Criteria Requête SolR
     */
    private Criteria buildPcpQuery(CriterionPcp pcp) {

        if (pcp.getPcp().size() > 0) {

            Iterator<String> pcpIterator = pcp.getPcp().iterator();
            String pcpCode = pcpIterator.next();

            Criteria myCriteria = new Criteria(NoticeField.PCP_S).is(pcpCode);

            while (pcpIterator.hasNext()) {
                pcpCode = pcpIterator.next();
                myCriteria = myCriteria.or(NoticeField.PCP_S).is(pcpCode);
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

            return myCriteria.connect();

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

            Criteria myCriteria = null;

            String rcrCode = rcrIterator.next();
            String rcrOperator = rcrOperatorIterator.next();

            // 1er critère
            switch (rcrOperator) {
                case LogicalOperator.EXCEPT:
                    myCriteria = new Criteria(NoticeField.RCR_S).is(rcrCode).not();
                    break;
                default:
                    myCriteria = new Criteria(NoticeField.RCR_S).is(rcrCode);
                    break;
            }

            // les autres
            while (rcrIterator.hasNext()) {
                rcrCode = rcrIterator.next();
                rcrOperator = rcrOperatorIterator.next();

                switch (rcrOperator) {
                    case LogicalOperator.AND:
                        myCriteria = myCriteria.and(NoticeField.RCR_S).is(rcrCode);
                        break;
                    case LogicalOperator.OR:
                        myCriteria = myCriteria.or(NoticeField.RCR_S).is(rcrCode);
                        break;
                    case LogicalOperator.EXCEPT:
                        myCriteria = myCriteria.and(NoticeField.RCR_S).is(rcrCode).not();
                        break;
                }
            }

            // pour le bloc entier
            switch (rcr.getBlocOperator()) {
                case LogicalOperator.AND:
                    myCriteria = myCriteria.connect();
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
     * Construit la requête SolR à partir d'un critère de recherche par mots du titre
     * @param titleWords Les critères de recherche par mots du titre
     * @return Criteria Requête SolR
     * @exception IllegalCriterionException Si la liste des critères est vide.
     */
    private Criteria buildTitleWordsQuery(CriterionTitleWords titleWords) throws IllegalCriterionException {

        if (titleWords.getTitleWords().isEmpty()) {
            throw new IllegalCriterionException("Criteria list cannot be empty");
        }

        Iterator<String> valueIterator = titleWords.getTitleWords().iterator();
        Iterator<String> operatorIterator = titleWords.getTitleWordsOperator().iterator();

        Criteria myCriteria = null;

        String value = valueIterator.next();
        String operator = operatorIterator.next();

        // 1er critère
        switch (operator) {
            case LogicalOperator.EXCEPT:
                myCriteria = new Criteria(NoticeField.KEY_TITLE_T).is(value).not().
                        or(NoticeField.KEY_SHORTED_TITLE_T).is(value).not().
                        or(NoticeField.PROPER_TITLE_T).is(value).not().
                        or(NoticeField.TITLE_FROM_DIFFERENT_AUTHOR_T).is(value).not().
                        or(NoticeField.PARALLEL_TITLE_T).is(value).not().
                        or(NoticeField.TITLE_COMPLEMENT_T).is(value).not().
                        or(NoticeField.SECTION_TITLE_T).is(value).not().connect();
                break;
            default:
                myCriteria = new Criteria(NoticeField.KEY_TITLE_T).is(value).
                        or(NoticeField.KEY_SHORTED_TITLE_T).is(value).
                        or(NoticeField.PROPER_TITLE_T).is(value).
                        or(NoticeField.TITLE_FROM_DIFFERENT_AUTHOR_T).is(value).
                        or(NoticeField.PARALLEL_TITLE_T).is(value).
                        or(NoticeField.TITLE_COMPLEMENT_T).is(value).
                        or(NoticeField.SECTION_TITLE_T).is(value).connect();
                break;
        }

        // les autres
        while (valueIterator.hasNext()) {
            value = valueIterator.next();
            operator = operatorIterator.next();

            switch (operator) {
                case LogicalOperator.AND:
                    myCriteria = myCriteria.connect().and(NoticeField.KEY_TITLE_T).expression("\"*"+value+"\"").
                            or(NoticeField.KEY_SHORTED_TITLE_T).is(value).
                            or(NoticeField.PROPER_TITLE_T).is(value).
                            or(NoticeField.TITLE_FROM_DIFFERENT_AUTHOR_T).is(value).
                            or(NoticeField.PARALLEL_TITLE_T).is(value).
                            or(NoticeField.TITLE_COMPLEMENT_T).is(value).
                            or(NoticeField.SECTION_TITLE_T).is(value);
                    break;
                case LogicalOperator.OR:
                    myCriteria = myCriteria.connect().or(NoticeField.KEY_TITLE_T).expression("\"*"+value+"\"").
                            or(NoticeField.KEY_SHORTED_TITLE_T).is(value).
                            or(NoticeField.PROPER_TITLE_T).is(value).
                            or(NoticeField.TITLE_FROM_DIFFERENT_AUTHOR_T).is(value).
                            or(NoticeField.PARALLEL_TITLE_T).is(value).
                            or(NoticeField.TITLE_COMPLEMENT_T).is(value).
                            or(NoticeField.SECTION_TITLE_T).is(value);
                    break;
                case LogicalOperator.EXCEPT:
                    myCriteria = myCriteria.connect().and(NoticeField.KEY_TITLE_T).is(value).not().
                            or(NoticeField.KEY_SHORTED_TITLE_T).is(value).not().
                            or(NoticeField.PROPER_TITLE_T).is(value).not().
                            or(NoticeField.TITLE_FROM_DIFFERENT_AUTHOR_T).is(value).not().
                            or(NoticeField.PARALLEL_TITLE_T).is(value).not().
                            or(NoticeField.TITLE_COMPLEMENT_T).is(value).not().
                            or(NoticeField.SECTION_TITLE_T).is(value).not();
                    break;
            }
        }

        // pour le bloc entier
        switch (titleWords.getBlocOperator()) {
            case LogicalOperator.AND:
                break;
            case LogicalOperator.OR:
                myCriteria.setPartIsOr(true);
                break;
            case LogicalOperator.EXCEPT:
                myCriteria = myCriteria.notOperator();
                break;
        }

        return myCriteria;
    }


}
