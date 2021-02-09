package fr.abes.periscope.core.repository.solr;

import fr.abes.periscope.core.entity.PublicationYear;
import fr.abes.periscope.core.util.NoticeMapper;
import org.junit.Assert;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.text.ParseException;

/**
 * Test l'extraction des dates de publication de la zone 100$a d'une NoticeSolr.
 */
@SpringBootTest
public class NoticeMapperTest {

    @Autowired
    private NoticeMapper noticeMapper;

    /**
     * Test titre mort
     */
    @Test
    @DisplayName("Titre mort")
    public void testDeadTitle() throws ParseException {

        String input = "19970418b19972004k y0frey50 ba";
        int expectedStartYear = 1997;
        int expectedStartYearConfidenceIndex = 0;
        int expectedEndYear = 2004;
        int expectedEndYearConfidenceIndex = 0;

        PublicationYear startYear = noticeMapper.buildStartPublicationYear(input);
        PublicationYear endYear = noticeMapper.buildEndPublicationYear(input);

        Assert.assertEquals(expectedStartYear,startYear.getYear());
        Assert.assertEquals(expectedStartYearConfidenceIndex,startYear.getConfidenceIndex());

        Assert.assertEquals(expectedEndYear,endYear.getYear());
        Assert.assertEquals(expectedEndYearConfidenceIndex,endYear.getConfidenceIndex());
    }

}
