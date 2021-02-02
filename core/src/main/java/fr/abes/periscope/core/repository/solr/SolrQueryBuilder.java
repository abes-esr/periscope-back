package fr.abes.periscope.core.repository.solr;

import fr.abes.periscope.core.criterion.*;
import org.springframework.data.solr.core.query.Criteria;
import org.springframework.data.solr.core.query.FilterQuery;
import org.springframework.data.solr.core.query.SimpleFilterQuery;

import java.util.Iterator;
import java.util.List;

/**
 * Représente un constructeur de requête SolR pour Periscope
 */
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

            // bloc de critère ISSN
            if (criterion instanceof CriterionIssn) {
                Criteria issnQuery = buildIssnQuery((CriterionIssn)criterion);
                if (issnQuery != null) {
                    filterQuery.addCriteria(issnQuery);
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
     * Construit la requête SolR à partir d'un critère de recherche par ISSN
     * @param issn Les critères de recherche par ISSN
     * @return Criteria Requête SolR
     */
    private Criteria buildIssnQuery(CriterionIssn issn) {
        if (issn.getIssn().size() > 0) {

            Iterator<String> issnIterator = issn.getIssn().iterator();
            Iterator<String> issnOperatorIterator = issn.getIssnOperator().iterator();

            Criteria myCriteria;

            String issnCode = issnIterator.next();
            String issnOperator = issnOperatorIterator.next();

            // 1er critère
            switch (issnOperator) {
                case LogicalOperator.EXCEPT:
                    myCriteria = new Criteria(NoticeField.RCR_S).is(issnCode).not();
                    break;
                default:
                    myCriteria = new Criteria(NoticeField.RCR_S).is(issnCode);
                    break;
            }

            // les autres
            while (issnIterator.hasNext()) {
                issnCode = issnIterator.next();
                issnOperator = issnOperatorIterator.next();

                switch (issnOperator) {
                    case LogicalOperator.AND:
                        myCriteria = myCriteria.and(NoticeField.ISSN).is(issnCode);
                        break;
                    case LogicalOperator.OR:
                        myCriteria = myCriteria.or(NoticeField.ISSN).is(issnCode);
                        break;
                    case LogicalOperator.EXCEPT:
                        myCriteria = myCriteria.and(NoticeField.ISSN).is(issnCode).not();
                        break;
                }
            }

            // pour le bloc entier
            switch (issn.getBlocOperator()) {
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



}
