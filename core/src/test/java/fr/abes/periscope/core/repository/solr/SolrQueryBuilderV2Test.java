package fr.abes.periscope.core.repository.solr;

import com.sun.source.tree.AssertTree;
import fr.abes.periscope.core.CoreTestConfiguration;
import fr.abes.periscope.core.criterion.*;
import fr.abes.periscope.core.entity.v2.solr.NoticeV2SolrField;
import fr.abes.periscope.core.repository.solr.v2.SolrQueryBuilder;
import org.junit.Assert;
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
    void testConstructRequeteFacetteWithZoneBiblio() {
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
    void testConsructRequeteFacetteWithZoneExemplaire() {
        List<Criterion> criteresExemp = new LinkedList<>();

        List<String> pcp = Arrays.asList("PCAq", "PCAuv");
        List<String> pcpOperators = Arrays.asList("ET", "ET");
        CriterionPcp criterionPcp = new CriterionPcp(pcp, pcpOperators);
        criteresExemp.add(criterionPcp);

        FacetQuery query = builderQuery.constructFacetQuery(new LinkedList<>(), criteresExemp);
        DefaultQueryParser dqp = new DefaultQueryParser(null);
        String actualQuery = dqp.getQueryString(query, null);
        assertEquals(actualQuery, "{!parent which=notice_type:notice}(zone_930$z:PCAq OR zone_930$z:PCAuv)");

        List<String> rcr = Arrays.asList("123456789");
        List<String> rcrOperators = Arrays.asList("ET");
        CriterionRcr criterionRcr = new CriterionRcr(rcr, rcrOperators);
        criteresExemp.add(criterionRcr);

        query = builderQuery.constructFacetQuery(new LinkedList<>(), criteresExemp);
        actualQuery = dqp.getQueryString(query, null);
        assertEquals(actualQuery, "{!parent which=notice_type:notice}(zone_930$z:PCAq OR zone_930$z:PCAuv) AND (zone_930$b:123456789)");
    }

    @Test
    @DisplayName("Test construction requête à facette avec des zones de tous les niveaux")
    void testConstructRequeteFacetteWithZones() {
        List<Criterion> criteresNotices = new LinkedList<>();
        List<Criterion> criteresExemp = new LinkedList<>();

        List<String> ppn = Arrays.asList("123456789");
        String ppnOperators = "OU";
        CriterionPpn criterionPpn = new CriterionPpn(ppnOperators, ppn);
        criteresNotices.add(criterionPpn);


        List<String> pcp = Arrays.asList("PCAq", "PCAuv");
        List<String> pcpOperators = Arrays.asList("ET", "ET");
        CriterionPcp criterionPcp = new CriterionPcp(pcp, pcpOperators);
        criteresExemp.add(criterionPcp);

        FacetQuery query = builderQuery.constructFacetQuery(criteresNotices, criteresExemp);
        DefaultQueryParser dqp = new DefaultQueryParser(null);
        String actualQuery = dqp.getQueryString(query, null);
        assertEquals("{!parent which=notice_type:notice}(zone_930$z:PCAq OR zone_930$z:PCAuv)", actualQuery);
        assertEquals( "zone_001:123456789", dqp.getQueryString(query.getFilterQueries().get(0)));

    }

    @Test
    @DisplayName("Test ajout facettes")
    void testAjoutFacette(){
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

    @Test
    @DisplayName("test construction filtres facettes")
    void testFiltreFacette() {
        List<CriterionFacette> facettes = new LinkedList<>();
        facettes.add(new CriterionFacette("DOCUMENT_TYPE", "Périodiques"));
        facettes.add(new CriterionFacette("LANGUAGE", "FR"));

        FacetQuery query = new SimpleFacetQuery();
        query = builderQuery.addFacetsFilters(query, facettes);

        assertEquals(2, query.getFilterQueries().size());
        assertEquals("DOCUMENT_TYPE", query.getFilterQueries().get(0).getCriteria().getField().getName());
        assertEquals("Périodiques", query.getFilterQueries().get(0).getCriteria().getPredicates().iterator().next().getValue());
        assertEquals("LANGUAGE", query.getFilterQueries().get(1).getCriteria().getField().getName());
        assertEquals("FR", query.getFilterQueries().get(1).getCriteria().getPredicates().iterator().next().getValue());
    }

    @Test
    @DisplayName("test construction filtres facettes si vide")
    void testFiltreFacetteEmpty() {
        List<CriterionFacette> facettes = new LinkedList<>();

        FacetQuery query = new SimpleFacetQuery();
        query = builderQuery.addFacetsFilters(query, facettes);

        assertEquals(0, query.getFilterQueries().size());
    }
}
