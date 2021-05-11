package fr.abes.periscope.core.repository.solr;

import fr.abes.periscope.core.CoreTestConfiguration;
import fr.abes.periscope.core.EnableOnIntegrationTest;
import fr.abes.periscope.core.criterion.*;
import fr.abes.periscope.core.entity.Notice;
import fr.abes.periscope.core.entity.OnGoingResourceType;
import fr.abes.periscope.core.entity.v2.solr.NoticeV2SolrField;
import fr.abes.periscope.core.entity.v2.solr.ResultSolr;
import fr.abes.periscope.core.service.NoticeStoreService;
import org.junit.Assert;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Arrays;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

/**
 * Test la conversion d'une Notice SolR vers une Notice.
 */
@EnableOnIntegrationTest
@SpringBootTest(classes = {CoreTestConfiguration.class})
public class NoticeStoreServiceTest {

    @Autowired
    private NoticeStoreService noticeService;

    /**
     * Test Journal Le Monde
     */
    @Test
    @DisplayName("Journal Le Monde")
    void testLeMonde() {

        List<Criterion> criteria = new LinkedList<>();

        List<String> mots = Arrays.asList("le monde");
        List<String> operator = Arrays.asList("ET");
        CriterionTitleWords criterion = new CriterionTitleWords("ET",mots,operator);
        criteria.add(criterion);

        List<Notice> candidates = noticeService.findNoticesByCriteria(criteria, new LinkedList<>(), 0,5);

    }

    /**
     * Test Notice sans type de ressource continue
     */
    @Test
    @DisplayName("Notice sans type de ressource continue")
    void testNoticeWithOutResourceType() {
        String expectedType = OnGoingResourceType.X;

        List<Criterion> criteria = new LinkedList<>();

        List<String> ppn = Arrays.asList("155084690");
        CriterionPpn criterion = new CriterionPpn("ET",ppn);
        criteria.add(criterion);

        Notice candidate = noticeService.findNoticesByCriteria(criteria,  new LinkedList<>(),0,5).get(0);

        Assert.assertEquals(expectedType,candidate.getContiniousType());

    }

    /**
     * Test Notice avec lien Mirabel
     */
    @Test
    @DisplayName("Notice avec lien Mirabel")
    void testNoticeWithMirabelLink() {
        String expectedType = "https://reseau-mirabel.info/revue/titre-id/5072";

        List<Criterion> criteria = new LinkedList<>();

        List<String> ppn = Arrays.asList("056533667");
        CriterionPpn criterion = new CriterionPpn("ET",ppn);
        criteria.add(criterion);

        Notice candidate = noticeService.findNoticesByCriteria(criteria,  new LinkedList<>(),0,5).get(0);

        Assert.assertEquals(expectedType,candidate.getMirabelURL());
    }

    /**
     * Test Notice sans lien Mirabel
     */
    @Test
    @DisplayName("Notice sans lien Mirabel")
    void testNoticeWithOutMirabelLink() {
        String expectedType = null;

        List<Criterion> criteria = new LinkedList<>();

        List<String> ppn = Arrays.asList("070618755");
        CriterionPpn criterion = new CriterionPpn("ET",ppn);
        criteria.add(criterion);

        Notice candidate = noticeService.findNoticesByCriteria(criteria,  new LinkedList<>(),0,5).get(0);

        Assert.assertEquals(expectedType,candidate.getMirabelURL());
    }

    /**
     * Test Notice avec le nombre de localisation
     */
    @Test
    @DisplayName("Notice nombre de localisation")
    void testNoticeNbLoc() {
        Integer expected = 3;

        List<Criterion> criteria = new LinkedList<>();

        List<String> ppn = Arrays.asList("070618755");
        CriterionPpn criterion = new CriterionPpn("ET",ppn);
        criteria.add(criterion);

        Notice candidate = noticeService.findNoticesByCriteria(criteria,  new LinkedList<>(),0,5).get(0);

        Assert.assertEquals(expected,candidate.getNbLocation());
    }

    @Test
    @DisplayName("Fix bug NumberFormatException: For input string: \"19  \"")
    void testFixBug1() {
        List<Criterion> criteria = new LinkedList<>();
        List<String> rcr = Arrays.asList("661362104");
        List<String> rcr_operator = Arrays.asList("ET");
        CriterionRcr criterionRcr = new CriterionRcr("SAUF",rcr,rcr_operator);
        criteria.add(criterionRcr);

        List<Notice> newCandidates = noticeService.findNoticesByCriteria(criteria,  new LinkedList<>(),0,25);

    }

    /**
     * Test Journal Le Monde
     */
    @Test
    @DisplayName("Test entre le SolR V1 et V2")
    void testSolRV1V2() {

        List<Criterion> criteria = new LinkedList<>();

        List<String> ppn = Arrays.asList("000000191");
        CriterionPpn criterion = new CriterionPpn("ET",ppn);
        criteria.add(criterion);

        Notice candidate = noticeService.findNoticesByCriteria(criteria,  new LinkedList<>(),0,5).get(0);
        String expected = candidate.getProperTitle();

        List<Criterion> criteria1 = new LinkedList<>();
        List<String> ppnParent = Arrays.asList("000000191");
        CriterionPpnParent criterionPpnParent = new CriterionPpnParent("ET", ppnParent);
        criteria1.add(criterionPpnParent);

        candidate = noticeService.findNoticesByCriteria("v2",criteria1,  new LinkedList<>(),0,5).get(0);
        Assert.assertEquals(expected,candidate.getProperTitle());

    }

    @DisplayName("Test requête avec facettes")
    @Test
    void testFacet() {
        List<Criterion> criteresNotices = new LinkedList<>();
        List<Criterion> criteresExemp = new LinkedList<>();

        List<String> titleWord = Arrays.asList("monde");
        List<String> titleOperators = Arrays.asList("ET");
        CriterionTitleWords titleWords = new CriterionTitleWords(titleWord, titleOperators);
        criteresNotices.add(titleWords);

        List<String> rcr = Arrays.asList("341725201");
        List<String> rcrOperators = Arrays.asList("ET");
        CriterionRcr criterionRcr = new CriterionRcr(rcr, rcrOperators);
        criteresExemp.add(criterionRcr);

        List<String> facette = Arrays.asList(NoticeV2SolrField.DOCUMENT_TYPE, NoticeV2SolrField.NB_LOC);

        ResultSolr candidates = noticeService.findNoticesWithFacets(criteresNotices, facette, new LinkedList<>(), 0, 10);

        assertEquals(candidates.getFacettes().size(), 2);

    }
}
