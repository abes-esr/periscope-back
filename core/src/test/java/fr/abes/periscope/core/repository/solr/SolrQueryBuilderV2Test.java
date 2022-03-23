package fr.abes.periscope.core.repository.solr;

import fr.abes.periscope.core.CoreTestConfiguration;
import fr.abes.periscope.core.criterion.Criterion;
import fr.abes.periscope.core.criterion.CriterionPcp;
import fr.abes.periscope.core.criterion.CriterionPpn;
import fr.abes.periscope.core.criterion.CriterionRcr;
import fr.abes.periscope.core.entity.v2.solr.NoticeV2SolrField;
import fr.abes.periscope.core.repository.solr.v2.SolrQueryBuilder;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.solr.core.DefaultQueryParser;
import org.springframework.data.solr.core.query.FacetQuery;
import org.springframework.data.solr.core.query.SimpleFacetQuery;
import org.springframework.data.solr.core.query.SimpleField;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

@SpringBootTest(classes = {CoreTestConfiguration.class})
public class SolrQueryBuilderV2Test {
    private SolrQueryBuilder builderQuery;

    public SolrQueryBuilderV2Test(@Qualifier("SolrQueryV2Builder") fr.abes.periscope.core.repository.solr.v2.SolrQueryBuilder builder) {
        builderQuery = builder;
    }

    @Test
    @DisplayName("Test construction requête à facettes avec une zone biblio")
    public void testConstructRequeteFacetteWithZoneBiblio() {
        List<Criterion> criteresNotices = new LinkedList<>();

        List<String> ppn = Arrays.asList("123456789");
        String ppnOperators = "ET";
        CriterionPpn criterionPpn = new CriterionPpn(ppnOperators, ppn);
        criteresNotices.add(criterionPpn);

        FacetQuery query = builderQuery.constructFacetQuery(criteresNotices, new LinkedList<>());
        DefaultQueryParser dqp = new DefaultQueryParser(null);
        String actualQuery = dqp.getQueryString(query, null);
        assertEquals(actualQuery, "zone_001:123456789");
    }

    @Test
    @DisplayName("Test construction requête à facettes avec une zone exemplaire")
    @Disabled
    public void testConsructRequeteFacetteWithZoneExemplaire() {
        List<Criterion> criteresExemp = new LinkedList<>();

        List<String> pcp = Arrays.asList("PCAq", "PCAuv");
        List<String> pcpOperators = Arrays.asList("OU", "OU");
        CriterionPcp criterionPcp = new CriterionPcp(pcp, pcpOperators);
        criteresExemp.add(criterionPcp);

        FacetQuery query = builderQuery.constructFacetQuery(new LinkedList<>(), criteresExemp);
        DefaultQueryParser dqp = new DefaultQueryParser(null);
        String actualQuery = dqp.getQueryString(query, null);
        assertEquals(actualQuery, "{!parent which=notice_type:notice}pcpList:PCAq OR pcpList:PCAuv");

        List<String> rcr = Arrays.asList("123456789");
        List<String> rcrOperators = Arrays.asList("ET");
        CriterionRcr criterionRcr = new CriterionRcr(rcr, rcrOperators);
        criteresExemp.add(criterionRcr);

        query = builderQuery.constructFacetQuery(new LinkedList<>(), criteresExemp);
        actualQuery = dqp.getQueryString(query, null);
        assertEquals(actualQuery, "{!parent which=notice_type:notice}pcpList:PCAq OR pcpList:PCAuv AND rcrList:123456789");
    }

    @Test
    @DisplayName("Test construction requête à facette avec des zones de tous les niveaux")
    public void testConstructRequeteFacetteWithZones() {
        List<Criterion> criteresNotices = new LinkedList<>();
        List<Criterion> criteresExemp = new LinkedList<>();

        List<String> ppn = Arrays.asList("123456789");
        String ppnOperators = "OU";
        CriterionPpn criterionPpn = new CriterionPpn(ppnOperators, ppn);
        criteresNotices.add(criterionPpn);


        List<String> pcp = Arrays.asList("PCAq", "PCAuv");
        List<String> pcpOperators = Arrays.asList("ET", "ET");
        CriterionPcp criterionPcp = new CriterionPcp(pcp, pcpOperators);
        criteresNotices.add(criterionPcp);

        FacetQuery query = builderQuery.constructFacetQuery(criteresNotices, criteresExemp);
        DefaultQueryParser dqp = new DefaultQueryParser(null);
        String actualQuery = dqp.getQueryString(query, null);
        assertEquals("zone_001:123456789 AND (pcpList:PCAq AND pcpList:PCAuv)", actualQuery);

    }

    @Test
    @DisplayName("Test ajout facettes")
    public void testAjoutFacette(){
        List<String> facettes = new ArrayList<>();
        facettes.add("DOCUMENT_TYPE");
        FacetQuery query = new SimpleFacetQuery();
        query = builderQuery.addFacetsNotices(query, facettes);
        assertTrue(query.hasFacetOptions());
        assertTrue(query.getFacetOptions().getFacetOnFields().contains(new SimpleField(NoticeV2SolrField.DOCUMENT_TYPE)));

        facettes.add("NB_LOC");
        query = builderQuery.addFacetsNotices(query, facettes);
        assertTrue(query.hasFacetOptions());
        assertEquals(query.getFacetOptions().getFacetOnFields().size(), 2);
        assertTrue(query.getFacetOptions().getFacetOnFields().contains(new SimpleField(NoticeV2SolrField.NB_LOC)));
    }
}
