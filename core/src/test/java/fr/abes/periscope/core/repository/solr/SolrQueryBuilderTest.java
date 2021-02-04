package fr.abes.periscope.core.repository.solr;

import fr.abes.periscope.core.criterion.*;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.data.solr.core.DefaultQueryParser;
import org.springframework.data.solr.core.query.SimpleQuery;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Test la construction de requête SolR à partir des critères de recherche.
 */
@SpringBootTest(classes = SolrQueryBuilder.class)
public class SolrQueryBuilderTest {

    @Autowired
    private SolrQueryBuilder builderQuery;

    /**
     * Test de l'historiette #id 12
     */
    @Test
    @DisplayName("historiette #id 12")
    public void testId12() {

        List<Criterion> criteria = new LinkedList<>();

        List<String> pcp = Arrays.asList("PCCor");
        List<String> pcpOperator = Arrays.asList("OU");
        CriterionPcp criterionPcp = new CriterionPcp(pcp,pcpOperator);
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
    @DisplayName("historiette #id 201")
    public void testId201() {

        List<Criterion> criteria = new LinkedList<>();

        List<String> pcp = Arrays.asList("PCCor","PCPACA");
        List<String> pcpOperator = Arrays.asList("OU","OU");
        CriterionPcp criterionPcp = new CriterionPcp(pcp,pcpOperator);
        criteria.add(criterionPcp);

        SimpleQuery solrQuery = new SimpleQuery(builderQuery.buildQuery(criteria));

        DefaultQueryParser dqp = new DefaultQueryParser(null);
        String actualQuery = dqp.getQueryString(solrQuery, null);
        String expectedQuery =
                "(930-z_s:PCCor OR 930-z_s:PCPACA)";
        assertEquals(expectedQuery, actualQuery);
    }

    /**
     * Test de l'historiette #id202
     */
    @Test
    @DisplayName("historiette #id 202")
    public void testId202() {
        List<Criterion> criteria = new LinkedList<>();

        List<String> ppn = Arrays.asList("038640139");
        CriterionPpn criterionPpn = new CriterionPpn(ppn);
        criteria.add(criterionPpn);

        List<String> pcp = Arrays.asList("PCCor");
        List<String> pcpOperator = Arrays.asList("OU");
        CriterionPcp criterionPcp = new CriterionPcp("ET", pcp,pcpOperator);
        criteria.add(criterionPcp);

        SimpleQuery solrQuery = new SimpleQuery(builderQuery.buildQuery(criteria));
        DefaultQueryParser dqp = new DefaultQueryParser(null);
        String actualQuery = dqp.getQueryString(solrQuery, null);
        String expectedQuery =
                "001_s:038640139 AND (930-z_s:PCCor)";
        assertEquals(expectedQuery, actualQuery);
    }

    /**
     * Test de l'historiette #id203
     */
    @Test
    @DisplayName("historiette #id 203")
    public void testId203() {
        List<Criterion> criteria = new LinkedList<>();

        List<String> pcp = Arrays.asList("PCCor");
        List<String> pcpOperator = Arrays.asList("OU");
        CriterionPcp criterionPcp = new CriterionPcp("ET", pcp,pcpOperator);
        criteria.add(criterionPcp);

        List<String> ppn = Arrays.asList("038640140");
        CriterionPpn criterionPpn = new CriterionPpn("SAUF", ppn);
        criteria.add(criterionPpn);

        SimpleQuery solrQuery = new SimpleQuery(builderQuery.buildQuery(criteria));

        DefaultQueryParser dqp = new DefaultQueryParser(null);
        String actualQuery = dqp.getQueryString(solrQuery, null);
        String expectedQuery =
                "930-z_s:PCCor AND -(001_s:038640140)";
        assertEquals(expectedQuery, actualQuery);
    }

    /**
     * Test de l'historiette #id204
     */
    @Test
    @DisplayName("historiette #id 204")
    public void testId204() {
        List<Criterion> criteria = new LinkedList<>();

        List<String> pcp = Arrays.asList("PCCor");
        List<String> pcpOperator = Arrays.asList("OU");
        CriterionPcp criterionPcp = new CriterionPcp("ET", pcp,pcpOperator);
        criteria.add(criterionPcp);

        List<String> ppn = Arrays.asList("039612473");
        CriterionPpn criterionPpn = new CriterionPpn("OU", ppn);
        criteria.add(criterionPpn);

        SimpleQuery solrQuery = new SimpleQuery(builderQuery.buildQuery(criteria));
        DefaultQueryParser dqp = new DefaultQueryParser(null);
        String actualQuery = dqp.getQueryString(solrQuery, null);
        String expectedQuery =
                "930-z_s:PCCor OR 001_s:039612473";
        assertEquals(expectedQuery, actualQuery);
    }

    /**
     * Test de l'historiette #id210
     */
    @Test
    @DisplayName("historiette #id 210")
    public void testId210() {

        List<Criterion> criteria = new LinkedList<>();

        List<String> rcr = Arrays.asList("200336201","200962101");
        List<String> operator = Arrays.asList("ET","ET");
        CriterionRcr criterionRcr = new CriterionRcr(rcr,operator);
        criteria.add(criterionRcr);

        SimpleQuery solrQuery = new SimpleQuery(builderQuery.buildQuery(criteria));

        DefaultQueryParser dqp = new DefaultQueryParser(null);
        String actualQuery = dqp.getQueryString(solrQuery, null);
        String expectedQuery =
                "(930-b_s:200336201 AND 930-b_s:200962101)";
        assertEquals(expectedQuery, actualQuery);
    }

    /**
     * Test de l'historiette #id211
     */
    @Test
    @DisplayName("historiette #id 211")
    public void testId211() {

        List<Criterion> criteria = new LinkedList<>();

        List<String> rcr = Arrays.asList("200336201","200962101");
        List<String> operator = Arrays.asList("ET","OU");
        CriterionRcr criterionRcr = new CriterionRcr(rcr,operator);

        criteria.add(criterionRcr);

        SimpleQuery solrQuery = new SimpleQuery(builderQuery.buildQuery(criteria));

        DefaultQueryParser dqp = new DefaultQueryParser(null);
        String actualQuery = dqp.getQueryString(solrQuery, null);
        String expectedQuery =
                "(930-b_s:200336201 OR 930-b_s:200962101)";
        assertEquals(expectedQuery, actualQuery);
    }

    /**
     * Test de l'historiette #id212
     */
    @Test
    @DisplayName("historiette #id 212")
    public void testId212() {

        List<Criterion> criteria = new LinkedList<>();

        List<String> rcr = Arrays.asList("200336201","200962101");
        List<String> operator = Arrays.asList("ET","ET");
        CriterionRcr criterionRcr = new CriterionRcr(rcr,operator);

        criteria.add(criterionRcr);

        SimpleQuery solrQuery = new SimpleQuery(builderQuery.buildQuery(criteria));

        DefaultQueryParser dqp = new DefaultQueryParser(null);
        String actualQuery = dqp.getQueryString(solrQuery, null);
        String expectedQuery =
                "(930-b_s:200336201 AND 930-b_s:200962101)";
        assertEquals(expectedQuery, actualQuery);
    }

    /**
     * Test de l'historiette #id214
     */
    @Test
    @DisplayName("historiette #id 214")
    public void testId214() {

        List<Criterion> criteria = new LinkedList<>();

        List<String> pcp = Arrays.asList("PCCor");
        List<String> pcpOperator = Arrays.asList("OU");
        CriterionPcp criterionPcp = new CriterionPcp(pcp,pcpOperator);
        criteria.add(criterionPcp);

        List<String> titleWords = Arrays.asList("corse");
        List<String> operator = Arrays.asList("ET");
        CriterionTitleWords criterionTitleWords = new CriterionTitleWords("OU",titleWords,operator);
        criteria.add(criterionTitleWords);

        SimpleQuery solrQuery = new SimpleQuery(builderQuery.buildQuery(criteria));

        DefaultQueryParser dqp = new DefaultQueryParser(null);
        String actualQuery = dqp.getQueryString(solrQuery, null);
        String expectedQuery =
                "930-z_s:PCCor OR ((530-a_t:corse OR 531-a_t:corse OR 200-a_t:corse OR 200-c_t:corse OR 200-d_t:corse OR 200-e_t:corse OR 200-i_t:corse))";
        assertEquals(expectedQuery, actualQuery);
    }

    /**
     * Test de l'historiette #id215
     */
    @Test
    @DisplayName("historiette #id 215")
    public void testId215() {

        List<Criterion> criteria = new LinkedList<>();

        List<String> pcp = Arrays.asList("PCCor");
        List<String> pcpOperator = Arrays.asList("OU");
        CriterionPcp criterionPcp = new CriterionPcp(pcp,pcpOperator);
        criteria.add(criterionPcp);

        List<String> titleWords = Arrays.asList("corse");
        List<String> operator = Arrays.asList("ET");
        CriterionTitleWords criterionTitleWords = new CriterionTitleWords("ET",titleWords,operator);
        criteria.add(criterionTitleWords);

        SimpleQuery solrQuery = new SimpleQuery(builderQuery.buildQuery(criteria));

        DefaultQueryParser dqp = new DefaultQueryParser(null);
        String actualQuery = dqp.getQueryString(solrQuery, null);
        String expectedQuery =
                "930-z_s:PCCor AND ((530-a_t:corse OR 531-a_t:corse OR 200-a_t:corse OR 200-c_t:corse OR 200-d_t:corse OR 200-e_t:corse OR 200-i_t:corse))";
        assertEquals(expectedQuery, actualQuery);
    }

    /**
     * Test de l'historiette #id216
     */
    @Test
    @DisplayName("historiette #id 216")
    public void testId216() {

        List<Criterion> criteria = new LinkedList<>();

        List<String> pcp = Arrays.asList("PCCor");
        List<String> pcpOperator = Arrays.asList("OU");
        CriterionPcp criterionPcp = new CriterionPcp(pcp,pcpOperator);
        criteria.add(criterionPcp);

        List<String> titleWords = Arrays.asList("corse");
        List<String> operator = Arrays.asList("ET");
        CriterionTitleWords criterionTitleWords = new CriterionTitleWords("SAUF",titleWords,operator);
        criteria.add(criterionTitleWords);

        SimpleQuery solrQuery = new SimpleQuery(builderQuery.buildQuery(criteria));

        DefaultQueryParser dqp = new DefaultQueryParser(null);
        String actualQuery = dqp.getQueryString(solrQuery, null);
        String expectedQuery =
                "930-z_s:PCCor AND -((530-a_t:corse OR 531-a_t:corse OR 200-a_t:corse OR 200-c_t:corse OR 200-d_t:corse OR 200-e_t:corse OR 200-i_t:corse))";
        assertEquals(expectedQuery, actualQuery);
    }

    @Test
    @DisplayName("historiette #id226")
    public void testId226() {
        List<Criterion> criteria = new LinkedList<>();

        List<String> pcp = Arrays.asList("PCCor");
        List<String> pcpOperator = Arrays.asList("OU");
        CriterionPcp criterionPcp = new CriterionPcp(pcp,pcpOperator);
        criteria.add(criterionPcp);

        List<String> language = Arrays.asList("cos");
        List<String> operator = Arrays.asList("ET");
        CriterionLangue criterionLangue = new CriterionLangue("ET",language,operator);
        criteria.add(criterionLangue);

        SimpleQuery solrQuery = new SimpleQuery(builderQuery.buildQuery(criteria));

        DefaultQueryParser dqp = new DefaultQueryParser(null);
        String actualQuery = dqp.getQueryString(solrQuery, null);
        String expectedQuery =
                "930-z_s:PCCor AND ((101-a_t:cos AND 530-a_t:[* TO *]))";
        assertEquals(expectedQuery, actualQuery);

    }

    @Test
    @DisplayName("historiette #id227")
    public void testId227() {
        List<Criterion> criteria = new LinkedList<>();

        List<String> pcp = Arrays.asList("PCCor");
        List<String> pcpOperator = Arrays.asList("OU");
        CriterionPcp criterionPcp = new CriterionPcp(pcp,pcpOperator);
        criteria.add(criterionPcp);

        List<String> language = Arrays.asList("cos");
        List<String> operator = Arrays.asList("ET");
        CriterionLangue criterionLangue = new CriterionLangue("OU",language,operator);
        criteria.add(criterionLangue);

        SimpleQuery solrQuery = new SimpleQuery(builderQuery.buildQuery(criteria));

        DefaultQueryParser dqp = new DefaultQueryParser(null);
        String actualQuery = dqp.getQueryString(solrQuery, null);
        String expectedQuery =
                "930-z_s:PCCor OR (101-a_t:cos AND 530-a_t:[* TO *])";
        assertEquals(expectedQuery, actualQuery);
    }

    @Test
    @DisplayName("historiette #id228")
    public void testId228() {
        List<Criterion> criteria = new LinkedList<>();

        List<String> pcp = Arrays.asList("PCCor");
        List<String> pcpOperator = Arrays.asList("OU");
        CriterionPcp criterionPcp = new CriterionPcp(pcp,pcpOperator);
        criteria.add(criterionPcp);

        List<String> language = Arrays.asList("cos");
        List<String> operator = Arrays.asList("ET");
        CriterionLangue criterionLangue = new CriterionLangue("SAUF",language,operator);
        criteria.add(criterionLangue);

        SimpleQuery solrQuery = new SimpleQuery(builderQuery.buildQuery(criteria));

        DefaultQueryParser dqp = new DefaultQueryParser(null);
        String actualQuery = dqp.getQueryString(solrQuery, null);
        String expectedQuery =
                "930-z_s:PCCor AND -(101-a_t:cos AND 530-a_t:[* TO *])";
        assertEquals(expectedQuery, actualQuery);
    }
    /**
     * Test de l'historiette #idX1
     */
    @Test
    @DisplayName("historiette #id X1")
    public void testIdX1() {

        List<Criterion> criteria = new LinkedList<>();

        List<String> pcp = Arrays.asList("PCAq","PCAuv","PCBo","PCBre","PCCA","PCCAPI","PCCor","PCFC","PCLR","PCLim","PCLor","PCMP","PCNPDC","PCPACA","PCPCh","PCPL","PCPic","PCRA","PCSAM","PCSCen","PCUP","PCUR","PCAM","PCAS","PCAnt","PCChimie","PCDroit","PCEBCO","PCGer","PCGéo","PCIta","PCMath","PCMed","PCMedieval","PCNum","PCPhilo","PCPhy", "PCPsy", "PCSTAPS");
        List<String> pcpOperator = Arrays.asList("OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU");
        CriterionPcp criterionPcp = new CriterionPcp(pcp,pcpOperator);
        criteria.add(criterionPcp);

        List<String> rcr = Arrays.asList("751052105");
        List<String> operator = Arrays.asList("ET");
        CriterionRcr criterionRcr = new CriterionRcr("SAUF",rcr,operator);
        criteria.add(criterionRcr);

        SimpleQuery solrQuery = new SimpleQuery(builderQuery.buildQuery(criteria));

        DefaultQueryParser dqp = new DefaultQueryParser(null);
        String actualQuery = dqp.getQueryString(solrQuery, null);
        String expectedQuery =
                "(930-z_s:PCAq OR 930-z_s:PCAuv OR 930-z_s:PCBo OR 930-z_s:PCBre OR 930-z_s:PCCA OR 930-z_s:PCCAPI OR 930-z_s:PCCor OR 930-z_s:PCFC OR 930-z_s:PCLR OR 930-z_s:PCLim OR 930-z_s:PCLor OR 930-z_s:PCMP OR 930-z_s:PCNPDC OR 930-z_s:PCPACA OR 930-z_s:PCPCh OR 930-z_s:PCPL OR 930-z_s:PCPic OR 930-z_s:PCRA OR 930-z_s:PCSAM OR 930-z_s:PCSCen OR 930-z_s:PCUP OR 930-z_s:PCUR OR 930-z_s:PCAM OR 930-z_s:PCAS OR 930-z_s:PCAnt OR 930-z_s:PCChimie OR 930-z_s:PCDroit OR 930-z_s:PCEBCO OR 930-z_s:PCGer OR 930-z_s:PCGéo OR 930-z_s:PCIta OR 930-z_s:PCMath OR 930-z_s:PCMed OR 930-z_s:PCMedieval OR 930-z_s:PCNum OR 930-z_s:PCPhilo OR 930-z_s:PCPhy OR 930-z_s:PCPsy OR 930-z_s:PCSTAPS) AND -(930-b_s:751052105)";
        assertEquals(expectedQuery, actualQuery);
    }


    /**
     * Test de l'historiette #idX2
     */
    @Test
    @DisplayName("historiette #id X2")
    public void testIdX2() {

        List<Criterion> criteria = new LinkedList<>();

        List<String> pcp = Arrays.asList("PCAq","PCAuv","PCBo","PCBre","PCCA","PCCAPI","PCCor","PCFC","PCLR","PCLim","PCLor","PCMP","PCNPDC","PCPACA","PCPCh","PCPL","PCPic","PCRA","PCSAM","PCSCen","PCUP","PCUR","PCAM","PCAS","PCAnt","PCChimie","PCDroit","PCEBCO","PCGer","PCGéo","PCIta","PCMath","PCMed","PCMedieval","PCNum","PCPhilo","PCPhy", "PCPsy", "PCSTAPS");
        List<String> pcpOperator = Arrays.asList("OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU");
        CriterionPcp criterionPcp = new CriterionPcp(pcp,pcpOperator);
        criteria.add(criterionPcp);

        List<String> rcr = Arrays.asList("674821001");
        List<String> operator = Arrays.asList("ET");
        CriterionRcr criterionRcr = new CriterionRcr("ET",rcr,operator);
        criteria.add(criterionRcr);

        SimpleQuery solrQuery = new SimpleQuery(builderQuery.buildQuery(criteria));
        DefaultQueryParser dqp = new DefaultQueryParser(null);
        String actualQuery = dqp.getQueryString(solrQuery, null);
        String expectedQuery =
                "(930-z_s:PCAq OR 930-z_s:PCAuv OR 930-z_s:PCBo OR 930-z_s:PCBre OR 930-z_s:PCCA OR 930-z_s:PCCAPI OR 930-z_s:PCCor OR 930-z_s:PCFC OR 930-z_s:PCLR OR 930-z_s:PCLim OR 930-z_s:PCLor OR 930-z_s:PCMP OR 930-z_s:PCNPDC OR 930-z_s:PCPACA OR 930-z_s:PCPCh OR 930-z_s:PCPL OR 930-z_s:PCPic OR 930-z_s:PCRA OR 930-z_s:PCSAM OR 930-z_s:PCSCen OR 930-z_s:PCUP OR 930-z_s:PCUR OR 930-z_s:PCAM OR 930-z_s:PCAS OR 930-z_s:PCAnt OR 930-z_s:PCChimie OR 930-z_s:PCDroit OR 930-z_s:PCEBCO OR 930-z_s:PCGer OR 930-z_s:PCGéo OR 930-z_s:PCIta OR 930-z_s:PCMath OR 930-z_s:PCMed OR 930-z_s:PCMedieval OR 930-z_s:PCNum OR 930-z_s:PCPhilo OR 930-z_s:PCPhy OR 930-z_s:PCPsy OR 930-z_s:PCSTAPS) AND (930-b_s:674821001)";
        assertEquals(expectedQuery, actualQuery);
    }

    /**
     * Test de l'historiette #idX3
     */
    @Test
    @DisplayName("historiette #id X3")
    public void testIdX3() {

        List<Criterion> criteria = new LinkedList<>();

        List<String> pcp = Arrays.asList("PCDroit");
        List<String> pcpOperator = Arrays.asList("OU");
        CriterionPcp criterionPcp = new CriterionPcp(pcp,pcpOperator);
        criteria.add(criterionPcp);

        List<String> rcr = Arrays.asList("212312101","341722102");
        List<String> operator = Arrays.asList("ET","OU");
        CriterionRcr criterionRcr = new CriterionRcr("OU",rcr,operator);

        criteria.add(criterionRcr);

        SimpleQuery solrQuery = new SimpleQuery(builderQuery.buildQuery(criteria));

        DefaultQueryParser dqp = new DefaultQueryParser(null);
        String actualQuery = dqp.getQueryString(solrQuery, null);
        String expectedQuery =
                "930-z_s:PCDroit OR (930-b_s:212312101 OR 930-b_s:341722102)";
        assertEquals(expectedQuery, actualQuery);
    }

    /**
     * Test de l'historiette #idX4
     */
    @Test
    @DisplayName("historiette #id X4")
    public void testIdX4() {

        List<Criterion> criteria = new LinkedList<>();

        List<String> pcp = Arrays.asList("PCDroit","PCPhilo");
        List<String> pcpOperator = Arrays.asList("OU","OU");
        CriterionPcp criterionPcp = new CriterionPcp(pcp,pcpOperator);
        criteria.add(criterionPcp);

        List<String> rcr = Arrays.asList("212312101","341722102");
        List<String> operator = Arrays.asList("ET","OU");
        CriterionRcr criterionRcr = new CriterionRcr("OU",rcr,operator);

        criteria.add(criterionRcr);

        SimpleQuery solrQuery = new SimpleQuery(builderQuery.buildQuery(criteria));

        DefaultQueryParser dqp = new DefaultQueryParser(null);
        String actualQuery = dqp.getQueryString(solrQuery, null);
        String expectedQuery =
                "(930-z_s:PCDroit OR 930-z_s:PCPhilo) OR (930-b_s:212312101 OR 930-b_s:341722102)";
        assertEquals(expectedQuery, actualQuery);
    }
}
