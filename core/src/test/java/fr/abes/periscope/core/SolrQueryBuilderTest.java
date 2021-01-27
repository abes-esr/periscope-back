package fr.abes.periscope.core;

import fr.abes.periscope.core.criterion.Criterion;
import fr.abes.periscope.core.criterion.CriterionPcp;
import fr.abes.periscope.core.repository.solr.SolrQueryBuilder;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.autoconfigure.domain.EntityScan;
import org.springframework.data.solr.core.DefaultQueryParser;
import org.springframework.data.solr.core.query.SimpleQuery;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Test la construction de requête SolR à partir des critères de recherche.
 */
@ExtendWith(SpringExtension.class)
@EnableAutoConfiguration
@ContextConfiguration(classes = SolrQueryBuilder.class)
public class SolrQueryBuilderTest {

    @Autowired
    protected SolrQueryBuilder builderQuery;

    /**
     * Test de l'historiette #id 12
     */
    @Test
    public void oneCriterionId12() {

        List<Criterion> criteria = new LinkedList<>();

        List<String> pcp = Arrays.asList("PCCor");
        CriterionPcp criterionPcp = new CriterionPcp("ET",pcp);

        criteria.add(criterionPcp);

        SimpleQuery solrQuery = new SimpleQuery(builderQuery.buildQuery(criteria));

        DefaultQueryParser dqp = new DefaultQueryParser(null);
        String actualQuery = dqp.getQueryString(solrQuery, null);
        String expectedQuery =
                "930-z_s:PCCor";
        assertEquals(expectedQuery, actualQuery);
    }

    /**
     * Test de l'historiette #id201
     */
    @Test
    public void twoCriteriaId201() {

        List<Criterion> criteria = new LinkedList<>();

        List<String> pcp = Arrays.asList("PCCor","PCPACA");
        CriterionPcp criterionPcp = new CriterionPcp("OU",pcp);

        criteria.add(criterionPcp);

        SimpleQuery solrQuery = new SimpleQuery(builderQuery.buildQuery(criteria));

        DefaultQueryParser dqp = new DefaultQueryParser(null);
        String actualQuery = dqp.getQueryString(solrQuery, null);
        String expectedQuery =
                "(930-z_s:PCCor OR 930-z_s:PCPACA)";
        assertEquals(expectedQuery, actualQuery);
    }
}
