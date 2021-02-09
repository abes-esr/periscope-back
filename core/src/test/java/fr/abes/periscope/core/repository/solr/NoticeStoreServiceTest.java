package fr.abes.periscope.core.repository.solr;

import fr.abes.periscope.core.EnableOnIntegrationTest;
import fr.abes.periscope.core.criterion.Criterion;
import fr.abes.periscope.core.criterion.CriterionPpn;
import fr.abes.periscope.core.criterion.CriterionTitleWords;
import fr.abes.periscope.core.entity.Notice;
import fr.abes.periscope.core.entity.OnGoingResourceType;
import fr.abes.periscope.core.service.NoticeStoreService;
import org.junit.Assert;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

/**
 * Test la conversion d'une Notice SolR vers une Notice.
 */
@EnableOnIntegrationTest
@SpringBootTest
public class NoticeStoreServiceTest {

    @Autowired
    private NoticeStoreService noticeService;

    /**
     * Test Journal Le Monde
     */
    @Test
    @DisplayName("Journal Le Monde")
    public void testLeMonde() {

        List<Criterion> criteria = new LinkedList<>();

        List<String> mots = Arrays.asList("le monde");
        List<String> operator = Arrays.asList("ET");
        CriterionTitleWords criterion = new CriterionTitleWords("ET",mots,operator);
        criteria.add(criterion);

        List<Notice> candidates = noticeService.findNoticesByCriteria(criteria, 0,5);
    }

    /**
     * Test Notice sans type de ressource continue
     */
    @Test
    @DisplayName("Notice sans type de ressource continue")
    public void testNoticeWithOutResourceType() {
        String expectedType = OnGoingResourceType.X;

        List<Criterion> criteria = new LinkedList<>();

        List<String> ppn = Arrays.asList("155084690");
        CriterionPpn criterion = new CriterionPpn("ET",ppn);
        criteria.add(criterion);

        Notice candidate = noticeService.findNoticesByCriteria(criteria, 0,5).get(0);

        Assert.assertEquals(expectedType,candidate.getContiniousType());

    }

    /**
     * Test Notice avec lien Mirabel
     */
    @Test
    @DisplayName("Notice avec lien Mirabel")
    public void testNoticeWithMirabelLink() {
        String expectedType = "https://reseau-mirabel.info/revue/titre-id/5072";

        List<Criterion> criteria = new LinkedList<>();

        List<String> ppn = Arrays.asList("056533667");
        CriterionPpn criterion = new CriterionPpn("ET",ppn);
        criteria.add(criterion);

        Notice candidate = noticeService.findNoticesByCriteria(criteria, 0,5).get(0);

        Assert.assertEquals(expectedType,candidate.getMirabelURL());
    }

    /**
     * Test Notice sans lien Mirabel
     */
    @Test
    @DisplayName("Notice sans lien Mirabel")
    public void testNoticeWithOutMirabelLink() {
        String expectedType = null;

        List<Criterion> criteria = new LinkedList<>();

        List<String> ppn = Arrays.asList("070618755");
        CriterionPpn criterion = new CriterionPpn("ET",ppn);
        criteria.add(criterion);

        Notice candidate = noticeService.findNoticesByCriteria(criteria, 0,5).get(0);

        Assert.assertEquals(expectedType,candidate.getMirabelURL());
    }

    /**
     * Test Notice avec le nombre de localisation
     */
    @Test
    @DisplayName("Notice nombre de localisation")
    public void testNoticeNbLoc() {
        Integer expected = 3;

        List<Criterion> criteria = new LinkedList<>();

        List<String> ppn = Arrays.asList("070618755");
        CriterionPpn criterion = new CriterionPpn("ET",ppn);
        criteria.add(criterion);

        Notice candidate = noticeService.findNoticesByCriteria(criteria, 0,5).get(0);

        Assert.assertEquals(expected,candidate.getNbLocation());
    }

}
