package fr.abes.periscope.core.util;

import fr.abes.periscope.core.entity.solr.Notice;
import fr.abes.periscope.core.entity.solr.OnGoingResourceType;
import fr.abes.periscope.core.entity.solr.PublicationYear;
import fr.abes.periscope.core.entity.solr.v2.NoticeV2;
import fr.abes.periscope.core.entity.solr.v2.ItemSolr;
import fr.abes.periscope.core.entity.solr.v2.NoticeV2Solr;
import fr.abes.periscope.core.exception.IllegalPublicationYearException;
import org.assertj.core.util.Lists;
import org.assertj.core.util.Sets;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.HashSet;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Test l'extraction des dates de publication de la zone 100$a d'une NoticeSolr.
 */
@SpringBootTest(classes = {UtilsMapper.class, NoticeSolRMapper.class})
class UtilsMapperTest {
    @Autowired
    private UtilsMapper mapper;
    @Autowired
    private NoticeSolRMapper noticeMapper;

    /**
     * Test titre mort
     */
    @Test
    @DisplayName("Titre mort")
    void testDeadTitle() {

        String input = "19970418b19972004k y0frey50 ba";
        String expectedStartYear = "1997";
        Integer expectedStartYearConfidenceIndex = 0;
        String expectedEndYear = "2004";
        Integer expectedEndYearConfidenceIndex = 0;

        PublicationYear startYear = mapper.buildStartPublicationYear(input);
        PublicationYear endYear = mapper.buildEndPublicationYear(input);

        Assertions.assertEquals(expectedStartYear, startYear.getYear());
        Assertions.assertEquals(expectedStartYearConfidenceIndex, startYear.getConfidenceIndex());

        Assertions.assertEquals(expectedEndYear, endYear.getYear());
        Assertions.assertEquals(expectedEndYearConfidenceIndex, endYear.getConfidenceIndex());
    }

    /**
     * Test titre mort avec année de début approximative (décennie)
     */
    @Test
    @DisplayName("Titre mort avec année de début approximative (décennie)")
    void testDeatTitleWithApproximateStartYear1() {
        String input = "19970418b199 2004k y0frey50 ba";
        String expectedStartYear = "199X";
        Integer expectedStartYearConfidenceIndex = 10;
        String expectedEndYear = "2004";
        Integer expectedEndYearConfidenceIndex = 0;

        PublicationYear startYear = mapper.buildStartPublicationYear(input);
        PublicationYear endYear = mapper.buildEndPublicationYear(input);

        Assertions.assertEquals(expectedStartYear, startYear.getYear());
        Assertions.assertEquals(expectedStartYearConfidenceIndex, startYear.getConfidenceIndex());

        Assertions.assertEquals(expectedEndYear, endYear.getYear());
        Assertions.assertEquals(expectedEndYearConfidenceIndex, endYear.getConfidenceIndex());
    }

    /**
     * Test titre mort avec année de début approximative (siècle)
     */
    @Test
    @DisplayName("Titre mort avec année de début approximative (siècle)")
    void testDeatTitleWithApproximateStartYear2() {
        String input = "19970418b19  2004k y0frey50 ba";
        String expectedStartYear = "19XX";
        Integer expectedStartYearConfidenceIndex = 100;
        String expectedEndYear = "2004";
        Integer expectedEndYearConfidenceIndex = 0;

        PublicationYear startYear = mapper.buildStartPublicationYear(input);
        PublicationYear endYear = mapper.buildEndPublicationYear(input);

        Assertions.assertEquals(expectedStartYear, startYear.getYear());
        Assertions.assertEquals(expectedStartYearConfidenceIndex, startYear.getConfidenceIndex());

        Assertions.assertEquals(expectedEndYear, endYear.getYear());
        Assertions.assertEquals(expectedEndYearConfidenceIndex, endYear.getConfidenceIndex());
    }

    /**
     * Test titre mort avec année de fin approximative (décennie)
     */
    @Test
    @DisplayName("Titre mort avec année de fin approximative (décennie)")
    void testDeatTitleWithApproximateEndYear1() {
        String input = "19970418b1997200 k y0frey50 ba";
        String expectedStartYear = "1997";
        Integer expectedStartYearConfidenceIndex = 0;
        String expectedEndYear = "200X";
        Integer expectedEndYearConfidenceIndex = 10;

        PublicationYear startYear = mapper.buildStartPublicationYear(input);
        PublicationYear endYear = mapper.buildEndPublicationYear(input);

        Assertions.assertEquals(expectedStartYear, startYear.getYear());
        Assertions.assertEquals(expectedStartYearConfidenceIndex, startYear.getConfidenceIndex());

        Assertions.assertEquals(expectedEndYear, endYear.getYear());
        Assertions.assertEquals(expectedEndYearConfidenceIndex, endYear.getConfidenceIndex());
    }

    /**
     * Test titre mort avec année de fin approximative (siècle)
     */
    @Test
    @DisplayName("Titre mort avec année de fin approximative (siècle)")
    void testDeatTitleWithApproximateEndYear2() {
        String input = "19970418b199720  k y0frey50 ba";
        String expectedStartYear = "1997";
        Integer expectedStartYearConfidenceIndex = 0;
        String expectedEndYear = "20XX";
        Integer expectedEndYearConfidenceIndex = 100;

        PublicationYear startYear = mapper.buildStartPublicationYear(input);
        PublicationYear endYear = mapper.buildEndPublicationYear(input);

        Assertions.assertEquals(expectedStartYear, startYear.getYear());
        Assertions.assertEquals(expectedStartYearConfidenceIndex, startYear.getConfidenceIndex());

        Assertions.assertEquals(expectedEndYear, endYear.getYear());
        Assertions.assertEquals(expectedEndYearConfidenceIndex, endYear.getConfidenceIndex());
    }

    /**
     * Test ressource continue en cours
     */
    @DisplayName("Ressource continue en cours")
    @Test
    void testOngoingResource() {
        String input = "19970418a19979999k y0frey50 ba";
        String expectedStartYear = "1997";
        Integer expectedStartYearConfidenceIndex = 0;

        PublicationYear startYear = mapper.buildStartPublicationYear(input);
        PublicationYear endYear = mapper.buildEndPublicationYear(input);

        Assertions.assertEquals(expectedStartYear, startYear.getYear());
        Assertions.assertEquals(expectedStartYearConfidenceIndex, startYear.getConfidenceIndex());

        Assertions.assertNull(endYear.getYear());
    }

    /**
     * Test ressource continue en cours avec exception
     */
    @DisplayName("Ressource continue en cours avec exception")
    @Test
    void testOngoingResourceWithException() {
        String input = "19970418a19972000k y0frey50 ba";
        Assertions.assertThrows(IllegalPublicationYearException.class, () -> mapper.buildEndPublicationYear(input));
    }

    /**
     * Test ressource continue dont la situation est inconnue
     */
    @DisplayName("ressource continue dont la situation est inconnue")
    @Test
    void testOnGoingResourceWithUnknownSituation() {
        String input = "19970418c1997    k y0frey50 ba";
        String expectedStartYear = "1997";
        Integer expectedStartYearConfidenceIndex = 0;

        PublicationYear startYear = mapper.buildStartPublicationYear(input);
        PublicationYear endYear = mapper.buildEndPublicationYear(input);

        Assertions.assertEquals(expectedStartYear, startYear.getYear());
        Assertions.assertEquals(expectedStartYearConfidenceIndex, startYear.getConfidenceIndex());

        Assertions.assertNull(endYear.getYear());
    }

    /**
     * Test ressource continue en cours dont la situation est inconnue avec exception
     */
    @DisplayName("Ressource continue en cours dont la situation est inconnue avec exception")
    @Test
    void testOnGoingResourceWithUnknownSituationWithException() {
        String input = "19970418c19972000k y0frey50 ba";
        Assertions.assertThrows(IllegalPublicationYearException.class, () -> mapper.buildEndPublicationYear(input));
    }

    /**
     * Test reproduction
     */
    @DisplayName("reproduction")
    @Test
    void testReproduction() {
        String input = "19970418e19972000k y0frey50 ba";
        String expectedStartYear = "1997";
        Integer expectedStartYearConfidenceIndex = 0;

        PublicationYear startYear = mapper.buildStartPublicationYear(input);
        PublicationYear endYear = mapper.buildEndPublicationYear(input);

        Assertions.assertEquals(expectedStartYear, startYear.getYear());
        Assertions.assertEquals(expectedStartYearConfidenceIndex, startYear.getConfidenceIndex());

        Assertions.assertNull(endYear.getYear());

    }

    /**
     * Test monographie dont la date de publication est incertaine
     */
    @DisplayName("monographie dont la date de publication est incertaine ")
    @Test
    void testMonographyWithUncertainDate() {
        String input = "19970418f19972000k y0frey50 ba";
        String expectedStartYear = "1997";
        Integer expectedStartYearConfidenceIndex = 3;

        PublicationYear startYear = mapper.buildStartPublicationYear(input);
        PublicationYear endYear = mapper.buildEndPublicationYear(input);

        Assertions.assertEquals(expectedStartYear, startYear.getYear());
        Assertions.assertEquals(expectedStartYearConfidenceIndex, startYear.getConfidenceIndex());

        Assertions.assertNull(endYear.getYear());
    }

    /**
     * Test monographie dont la date de publication est incertaine mais sans date de fin
     */
    @DisplayName("monographie dont la date de publication est incertaine sans date de fin")
    @Test
    void testMonographyWithUncertainDateAndNoEndDate() {
        String input = "19970418f1997    k y0frey50 ba";
        String expectedStartYear = "1997";
        Integer expectedStartYearConfidenceIndex = 0;

        PublicationYear startYear = mapper.buildStartPublicationYear(input);
        PublicationYear endYear = mapper.buildEndPublicationYear(input);

        Assertions.assertEquals(expectedStartYear, startYear.getYear());
        Assertions.assertEquals(expectedStartYearConfidenceIndex, startYear.getConfidenceIndex());

        Assertions.assertNull(endYear.getYear());
    }

    /**
     * Test monographie dont la publication s’étend sur plus d’une année
     */
    @DisplayName("monographie dont la publication s’étend sur plus d’une année")
    @Test
    void testMonographyOnManyYears() {
        String input = "19970418g19979999k y0frey50 ba";
        String expectedStartYear = "1997";
        Integer expectedStartYearConfidenceIndex = 0;

        PublicationYear startYear = mapper.buildStartPublicationYear(input);
        PublicationYear endYear = mapper.buildEndPublicationYear(input);

        Assertions.assertEquals(expectedStartYear, startYear.getYear());
        Assertions.assertEquals(expectedStartYearConfidenceIndex, startYear.getConfidenceIndex());

        Assertions.assertNull(endYear.getYear());
    }

    /**
     * Test monographie dont la publication s’étend sur plus d’une année encore en cours
     */
    @DisplayName("monographie dont la publication s’étend sur plus d’une année encore en cours")
    @Test
    void testMonographyOnManyYearsStillInProgress() {
        String input = "19970418g19971998k y0frey50 ba";
        String expectedStartYear = "1997";
        Integer expectedStartYearConfidenceIndex = 0;
        String expectedEndYear = "1998";
        Integer expectedEndYearConfidenceIndex = 0;

        PublicationYear startYear = mapper.buildStartPublicationYear(input);
        PublicationYear endYear = mapper.buildEndPublicationYear(input);

        Assertions.assertEquals(expectedStartYear, startYear.getYear());
        Assertions.assertEquals(expectedStartYearConfidenceIndex, startYear.getConfidenceIndex());

        Assertions.assertEquals(expectedEndYear, endYear.getYear());
        Assertions.assertEquals(expectedEndYearConfidenceIndex, endYear.getConfidenceIndex());
    }

    /**
     * Test Avec type de ressource continue
     */
    @DisplayName("avec type de ressource continue")
    @Test
    void testOnGoingResourceTypes() {
        String input = "aau|||||uu|";
        String expectedType = OnGoingResourceType.A;

        String actualType = noticeMapper.extractOnGoingResourceType(input);
        Assertions.assertEquals(expectedType,actualType);
    }

    /**
     * Test Sans type de ressource continue
     */
    @DisplayName("sans type de ressource continue")
    @Test
    void testWithOutOnGoingResourceTypes() {
        String expectedType = OnGoingResourceType.X;

        String actualType = noticeMapper.extractOnGoingResourceType(null);
        Assertions.assertEquals(expectedType,actualType);
    }

    @Test
    @DisplayName("Test conversion noticeV2Solr vers Notice")
    void testConverterNoticeV2SolrNotice() {
        NoticeV2Solr source = new NoticeV2Solr();
        source.setId("111111111");
        source.setTitleType("notice");
        source.setPpn("111111111");
        source.setIssn("1111-1111");
        String editor = "Test Editor";
        source.setEditor(Lists.newArrayList(editor));
        source.setEditorForDisplay(editor);
        source.setProcessingGlobalData("20081202a20099999uuuy0frey50      ba");
        String properTitle = "Test properTitle";
        source.setProperTitleForDisplay(properTitle);
        source.setProperTitle(Lists.newArrayList(properTitle));
        String titleFromDifferentAuthor = "Test titleFromDifferentAuthor";
        source.setTitleFromDifferentAuthorForDisplay(titleFromDifferentAuthor);
        source.setTitleFromDifferentAuthor(Lists.newArrayList(titleFromDifferentAuthor));
        String parallelTitle = "Test parallelTitle";
        source.setParallelTitleForDisplay(parallelTitle);
        source.setParallelTitle(Lists.newArrayList(parallelTitle));
        String titleComplement = "Test titleComplement";
        source.setTitleComplementForDisplay(titleComplement);
        source.setTitleComplement(Lists.newArrayList(titleComplement));
        String sectionTitle = "Test sectionTitle";
        source.setSectionTitleForDisplay(sectionTitle);
        source.setSectionTitle(Lists.newArrayList(sectionTitle));
        String keyTitle = "Test keyTitle";
        source.setKeyTitle(keyTitle);
        String keyTitleQualifier = "Test title qualifier";
        source.setKeyTitleQualifer(keyTitleQualifier);
        String keyShortedTitle = "test keyShortedTitle";
        source.setKeyShortedTitleForDisplay(keyShortedTitle);
        source.setKeyShortedTitle(Lists.newArrayList(keyShortedTitle));
        source.setContinuousType("Collection");
        source.setSupportType("Imprime");
        source.setExternalURLs(Lists.newArrayList("https://mirabel.com/"));
        source.setNbLocation(2);
        source.setNbPcp(1);
        source.setCountry("FR");
        source.setLanguage("fre");
        source.setStartYear("2020");
        source.setStartYearConfidenceIndex(0);
        source.setEndYear("202");
        source.setEndYearConfidenceIndex(1);
        source.setRcrList(Sets.newLinkedHashSet("341725201", "751050001"));
        source.setPcpList(Sets.newLinkedHashSet("PCMed"));
        ItemSolr item1 = new ItemSolr("111111111", "999999999");
        item1.setPcp(Lists.newArrayList("PCMed"));
        item1.setRcr("341725201");
        ItemSolr item2 = new ItemSolr("111111111", "888888888");
        item2.setPcp(Lists.newArrayList("PCMed"));
        item2.setRcr("751050001");
        Set<ItemSolr> items = new HashSet<>();
        items.add(item1);
        items.add(item2);
        source.setItems(items);

        Notice target = mapper.map(source, Notice.class);
        NoticeV2 noticeV2 = (NoticeV2) target;
        Assertions.assertEquals("111111111", noticeV2.getPpn());
        Assertions.assertEquals("1111-1111", noticeV2.getIssn());
        Assertions.assertEquals(editor, noticeV2.getPublisher());
        Assertions.assertEquals(keyTitle, noticeV2.getKeyTitle());
        Assertions.assertEquals("Collection", noticeV2.getContinuousType());
        Assertions.assertEquals("Imprime", noticeV2.getSupportType());
        Assertions.assertEquals("2020", noticeV2.getStartYear().getYear());
        Assertions.assertEquals("202", noticeV2.getEndYear().getYear());
        Assertions.assertEquals("https://mirabel.com/", noticeV2.getMirabelURL());
        Assertions.assertEquals(Integer.valueOf(2), noticeV2.getNbLocation());
        Assertions.assertEquals(keyShortedTitle, noticeV2.getKeyShortedTitle());
        Assertions.assertEquals(properTitle, noticeV2.getProperTitle());
        Assertions.assertEquals(titleFromDifferentAuthor, noticeV2.getTitleFromDifferentAuthor());
        Assertions.assertEquals(parallelTitle, noticeV2.getParallelTitle());
        Assertions.assertEquals(titleComplement, noticeV2.getTitleComplement());
        Assertions.assertEquals(sectionTitle, noticeV2.getSectionTitle());
        Assertions.assertEquals(keyTitleQualifier, noticeV2.getKeyTitleQualifer());
        Assertions.assertEquals("fre", noticeV2.getLanguage());
        Assertions.assertEquals("FR", noticeV2.getCountry());
        Assertions.assertEquals(Integer.valueOf(1), noticeV2.getNbPcp());
        Assertions.assertEquals(1, noticeV2.getPcpList().size());
        Assertions.assertEquals("PCMed", noticeV2.getPcpList().stream().findFirst().orElse(null));
        Assertions.assertEquals(2, noticeV2.getItems().size());
    }

    @Test
    @DisplayName("test construction titre notice")
    void testGetTitre() {

        assertEquals("titre clé",mapper.getTitre("titre clé",null,"",null,"","",""));
        assertEquals("titre clé titre clé qualifié",mapper.getTitre("titre clé","titre clé qualifié","",null,"","",""));
        assertEquals("titre clé court",mapper.getTitre(null,null,"titre clé court",null,"","",""));
        assertEquals("titre propre",mapper.getTitre(null,null,null,"titre propre","","",""));
        assertEquals("titre auteur different",mapper.getTitre(null,null,null,null,"titre auteur different","",""));
        assertEquals("titre parallele",mapper.getTitre(null,null,null,null,null,"titre parallele",""));
        assertEquals("titre complement",mapper.getTitre(null,null,null,null,null,null,"titre complement"));
    }
}
