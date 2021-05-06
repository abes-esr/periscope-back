package fr.abes.periscope.core.repository.solr;

import fr.abes.periscope.core.entity.OnGoingResourceType;
import fr.abes.periscope.core.entity.PublicationYear;
import fr.abes.periscope.core.exception.IllegalPublicationYearException;
import fr.abes.periscope.core.util.NoticeMapper;
import org.junit.Assert;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.text.ParseException;

/**
 * Test l'extraction des dates de publication de la zone 100$a d'une NoticeSolr.
 */
@SpringBootTest(classes = {NoticeMapper.class})
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
        Integer expectedStartYear = 1997;
        Integer expectedStartYearConfidenceIndex = 0;
        Integer expectedEndYear = 2004;
        Integer expectedEndYearConfidenceIndex = 0;

        PublicationYear startYear = noticeMapper.buildStartPublicationYear(input);
        PublicationYear endYear = noticeMapper.buildEndPublicationYear(input);

        Assert.assertEquals(expectedStartYear, startYear.getYear());
        Assert.assertEquals(expectedStartYearConfidenceIndex, startYear.getConfidenceIndex());

        Assert.assertEquals(expectedEndYear, endYear.getYear());
        Assert.assertEquals(expectedEndYearConfidenceIndex, endYear.getConfidenceIndex());
    }

    /**
     * Test titre mort avec année de début approximative (décennie)
     */
    @Test
    @DisplayName("Titre mort avec année de début approximative (décennie)")
    public void testDeatTitleWithApproximateStartYear1() throws ParseException {
        String input = "19970418b199 2004k y0frey50 ba";
        Integer expectedStartYear = 199;
        Integer expectedStartYearConfidenceIndex = 10;
        Integer expectedEndYear = 2004;
        Integer expectedEndYearConfidenceIndex = 0;

        PublicationYear startYear = noticeMapper.buildStartPublicationYear(input);
        PublicationYear endYear = noticeMapper.buildEndPublicationYear(input);

        Assert.assertEquals(expectedStartYear, startYear.getYear());
        Assert.assertEquals(expectedStartYearConfidenceIndex, startYear.getConfidenceIndex());

        Assert.assertEquals(expectedEndYear, endYear.getYear());
        Assert.assertEquals(expectedEndYearConfidenceIndex, endYear.getConfidenceIndex());
    }

    /**
     * Test titre mort avec année de début approximative (siècle)
     */
    @Test
    @DisplayName("Titre mort avec année de début approximative (siècle)")
    public void testDeatTitleWithApproximateStartYear2() throws ParseException {
        String input = "19970418b19  2004k y0frey50 ba";
        Integer expectedStartYear = 19;
        Integer expectedStartYearConfidenceIndex = 100;
        Integer expectedEndYear = 2004;
        Integer expectedEndYearConfidenceIndex = 0;

        PublicationYear startYear = noticeMapper.buildStartPublicationYear(input);
        PublicationYear endYear = noticeMapper.buildEndPublicationYear(input);

        Assert.assertEquals(expectedStartYear, startYear.getYear());
        Assert.assertEquals(expectedStartYearConfidenceIndex, startYear.getConfidenceIndex());

        Assert.assertEquals(expectedEndYear, endYear.getYear());
        Assert.assertEquals(expectedEndYearConfidenceIndex, endYear.getConfidenceIndex());
    }

    /**
     * Test titre mort avec année de fin approximative (décennie)
     */
    @Test
    @DisplayName("Titre mort avec année de fin approximative (décennie)")
    public void testDeatTitleWithApproximateEndYear1() throws ParseException {
        String input = "19970418b1997200 k y0frey50 ba";
        Integer expectedStartYear = 1997;
        Integer expectedStartYearConfidenceIndex = 0;
        Integer expectedEndYear = 200;
        Integer expectedEndYearConfidenceIndex = 10;

        PublicationYear startYear = noticeMapper.buildStartPublicationYear(input);
        PublicationYear endYear = noticeMapper.buildEndPublicationYear(input);

        Assert.assertEquals(expectedStartYear, startYear.getYear());
        Assert.assertEquals(expectedStartYearConfidenceIndex, startYear.getConfidenceIndex());

        Assert.assertEquals(expectedEndYear, endYear.getYear());
        Assert.assertEquals(expectedEndYearConfidenceIndex, endYear.getConfidenceIndex());
    }

    /**
     * Test titre mort avec année de fin approximative (siècle)
     */
    @Test
    @DisplayName("Titre mort avec année de fin approximative (siècle)")
    public void testDeatTitleWithApproximateEndYear2() throws ParseException {
        String input = "19970418b199720  k y0frey50 ba";
        Integer expectedStartYear = 1997;
        Integer expectedStartYearConfidenceIndex = 0;
        Integer expectedEndYear = 20;
        Integer expectedEndYearConfidenceIndex = 100;

        PublicationYear startYear = noticeMapper.buildStartPublicationYear(input);
        PublicationYear endYear = noticeMapper.buildEndPublicationYear(input);

        Assert.assertEquals(expectedStartYear, startYear.getYear());
        Assert.assertEquals(expectedStartYearConfidenceIndex, startYear.getConfidenceIndex());

        Assert.assertEquals(expectedEndYear, endYear.getYear());
        Assert.assertEquals(expectedEndYearConfidenceIndex, endYear.getConfidenceIndex());
    }

    /**
     * Test ressource continue en cours
     */
    @DisplayName("Ressource continue en cours")
    @Test
    public void testOngoingResource() throws ParseException {
        String input = "19970418a19979999k y0frey50 ba";
        Integer expectedStartYear = 1997;
        Integer expectedStartYearConfidenceIndex = 0;

        PublicationYear startYear = noticeMapper.buildStartPublicationYear(input);
        PublicationYear endYear = noticeMapper.buildEndPublicationYear(input);

        Assert.assertEquals(expectedStartYear, startYear.getYear());
        Assert.assertEquals(expectedStartYearConfidenceIndex, startYear.getConfidenceIndex());

        Assert.assertNull(endYear.getYear());
    }

    /**
     * Test ressource continue en cours avec exception
     */
    @DisplayName("Ressource continue en cours avec exception")
    @Test
    public void testOngoingResourceWithException() {
        String input = "19970418a19972000k y0frey50 ba";
        Assertions.assertThrows(IllegalPublicationYearException.class, () -> {
            noticeMapper.buildEndPublicationYear(input);
        });
    }

    /**
     * Test ressource continue dont la situation est inconnue
     */
    @DisplayName("ressource continue dont la situation est inconnue")
    @Test
    public void testOnGoingResourceWithUnknownSituation() throws ParseException {
        String input = "19970418c1997    k y0frey50 ba";
        Integer expectedStartYear = 1997;
        Integer expectedStartYearConfidenceIndex = 0;

        PublicationYear startYear = noticeMapper.buildStartPublicationYear(input);
        PublicationYear endYear = noticeMapper.buildEndPublicationYear(input);

        Assert.assertEquals(expectedStartYear, startYear.getYear());
        Assert.assertEquals(expectedStartYearConfidenceIndex, startYear.getConfidenceIndex());

        Assert.assertNull(endYear.getYear());
    }

    /**
     * Test ressource continue en cours dont la situation est inconnue avec exception
     */
    @DisplayName("Ressource continue en cours dont la situation est inconnue avec exception")
    @Test
    public void testOnGoingResourceWithUnknownSituationWithException() {
        String input = "19970418c19972000k y0frey50 ba";
        Assertions.assertThrows(IllegalPublicationYearException.class, () -> {
            noticeMapper.buildEndPublicationYear(input);
        });
    }

    /**
     * Test reproduction
     */
    @DisplayName("reproduction")
    @Test
    public void testReproduction() throws ParseException {
        String input = "19970418e19972000k y0frey50 ba";
        Integer expectedStartYear = 1997;
        Integer expectedStartYearConfidenceIndex = 0;

        PublicationYear startYear = noticeMapper.buildStartPublicationYear(input);
        PublicationYear endYear = noticeMapper.buildEndPublicationYear(input);

        Assert.assertEquals(expectedStartYear, startYear.getYear());
        Assert.assertEquals(expectedStartYearConfidenceIndex, startYear.getConfidenceIndex());

        Assert.assertNull(endYear.getYear());

    }

    /**
     * Test monographie dont la date de publication est incertaine
     */
    @DisplayName("monographie dont la date de publication est incertaine ")
    @Test
    public void testMonographyWithUncertainDate() throws ParseException {
        String input = "19970418f19972000k y0frey50 ba";
        Integer expectedStartYear = 1997;
        Integer expectedStartYearConfidenceIndex = 3;

        PublicationYear startYear = noticeMapper.buildStartPublicationYear(input);
        PublicationYear endYear = noticeMapper.buildEndPublicationYear(input);

        Assert.assertEquals(expectedStartYear, startYear.getYear());
        Assert.assertEquals(expectedStartYearConfidenceIndex, startYear.getConfidenceIndex());

        Assert.assertNull(endYear.getYear());
    }

    /**
     * Test monographie dont la date de publication est incertaine mais sans date de fin
     */
    @DisplayName("monographie dont la date de publication est incertaine sans date de fin")
    @Test
    public void testMonographyWithUncertainDateAndNoEndDate() throws ParseException {
        String input = "19970418f1997    k y0frey50 ba";
        Integer expectedStartYear = 1997;
        Integer expectedStartYearConfidenceIndex = 0;

        PublicationYear startYear = noticeMapper.buildStartPublicationYear(input);
        PublicationYear endYear = noticeMapper.buildEndPublicationYear(input);

        Assert.assertEquals(expectedStartYear, startYear.getYear());
        Assert.assertEquals(expectedStartYearConfidenceIndex, startYear.getConfidenceIndex());

        Assert.assertNull(endYear.getYear());
    }

    /**
     * Test monographie dont la publication s’étend sur plus d’une année
     */
    @DisplayName("monographie dont la publication s’étend sur plus d’une année")
    @Test
    public void testMonographyOnManyYears() throws ParseException {
        String input = "19970418g19979999k y0frey50 ba";
        Integer expectedStartYear = 1997;
        Integer expectedStartYearConfidenceIndex = 0;

        PublicationYear startYear = noticeMapper.buildStartPublicationYear(input);
        PublicationYear endYear = noticeMapper.buildEndPublicationYear(input);

        Assert.assertEquals(expectedStartYear, startYear.getYear());
        Assert.assertEquals(expectedStartYearConfidenceIndex, startYear.getConfidenceIndex());

        Assert.assertNull(endYear.getYear());
    }

    /**
     * Test monographie dont la publication s’étend sur plus d’une année encore en cours
     */
    @DisplayName("monographie dont la publication s’étend sur plus d’une année encore en cours")
    @Test
    public void testMonographyOnManyYearsStillInProgress() throws ParseException {
        String input = "19970418g19971998k y0frey50 ba";
        Integer expectedStartYear = 1997;
        Integer expectedStartYearConfidenceIndex = 0;
        Integer expectedEndYear = 1998;
        Integer expectedEndYearConfidenceIndex = 0;

        PublicationYear startYear = noticeMapper.buildStartPublicationYear(input);
        PublicationYear endYear = noticeMapper.buildEndPublicationYear(input);

        Assert.assertEquals(expectedStartYear, startYear.getYear());
        Assert.assertEquals(expectedStartYearConfidenceIndex, startYear.getConfidenceIndex());

        Assert.assertEquals(expectedEndYear, endYear.getYear());
        Assert.assertEquals(expectedEndYearConfidenceIndex, endYear.getConfidenceIndex());
    }

    /**
     * Test Avec type de ressource continue
     */
    @DisplayName("avec type de ressource continue")
    @Test
    public void testOnGoingResourceTypes() {
        String input = "aau|||||uu|";
        String expectedType = OnGoingResourceType.A;

        String actualType = noticeMapper.extractOnGoingResourceType(input);
        Assert.assertEquals(expectedType,actualType);
    }

    /**
     * Test Sans type de ressource continue
     */
    @DisplayName("sans type de ressource continue")
    @Test
    public void testWithOutOnGoingResourceTypes() {
        String input = null;
        String expectedType = OnGoingResourceType.X;

        String actualType = noticeMapper.extractOnGoingResourceType(input);
        Assert.assertEquals(expectedType,actualType);
    }
}
