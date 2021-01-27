package fr.abes.periscope.core.repository.solr;

import fr.abes.periscope.core.criterion.Criterion;
import fr.abes.periscope.core.criterion.CriterionPcp;
import fr.abes.periscope.core.criterion.CriterionRcr;
import fr.abes.periscope.core.criterion.LogicalOperator;
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


}
