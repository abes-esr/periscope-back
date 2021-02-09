package fr.abes.periscope.core.repository.solr;

import fr.abes.periscope.core.EnableOnIntegrationTest;
import fr.abes.periscope.core.criterion.Criterion;
import fr.abes.periscope.core.criterion.CriterionTitleWords;
import fr.abes.periscope.core.entity.Notice;
import fr.abes.periscope.core.service.NoticeStoreService;
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

}
