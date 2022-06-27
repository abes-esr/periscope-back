package fr.abes.periscope.core.util;

import com.fasterxml.jackson.dataformat.xml.JacksonXmlModule;
import com.fasterxml.jackson.dataformat.xml.XmlMapper;
import fr.abes.periscope.core.entity.solr.Notice;
import fr.abes.periscope.core.entity.solr.OnGoingResourceType;
import fr.abes.periscope.core.entity.solr.PublicationYear;
import fr.abes.periscope.core.entity.solr.ItemSolr;
import fr.abes.periscope.core.entity.solr.NoticeSolr;
import fr.abes.periscope.core.entity.xml.NoticeXml;
import fr.abes.periscope.core.exception.IllegalPublicationYearException;
import org.apache.commons.io.IOUtils;
import org.assertj.core.util.Lists;
import org.assertj.core.util.Sets;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.core.io.Resource;

import java.io.FileInputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
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

    @Value("classpath:noticeXml/037596225.xml")
    private Resource xmlFileOrphan;

    @Value("classpath:noticeXml/037982176.xml")
    private Resource xmlFilePC;

    @Value("classpath:noticeXml/13282261X.xml")
    private Resource xmlFile;

    @Value("classpath:noticeXml/129542059.xml")
    private Resource xmlFile2;

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
        NoticeSolr source = new NoticeSolr();
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
        source.setStatutList(Sets.newLinkedHashSet("PC"));
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
        Assertions.assertEquals("111111111", target.getPpn());
        Assertions.assertEquals("1111-1111", target.getIssn());
        Assertions.assertEquals(editor, target.getPublisher());
        Assertions.assertEquals(keyTitle, target.getKeyTitle());
        Assertions.assertEquals("Collection", target.getContinuousType());
        Assertions.assertEquals("Imprime", target.getSupportType());
        Assertions.assertEquals("2020", target.getStartYear().getYear());
        Assertions.assertEquals("202", target.getEndYear().getYear());
        Assertions.assertEquals("https://mirabel.com/", target.getMirabelURL());
        Assertions.assertEquals(Integer.valueOf(2), target.getNbLocation());
        Assertions.assertEquals(keyShortedTitle, target.getKeyShortedTitle());
        Assertions.assertEquals(properTitle, target.getProperTitle());
        Assertions.assertEquals(titleFromDifferentAuthor, target.getTitleFromDifferentAuthor());
        Assertions.assertEquals(parallelTitle, target.getParallelTitle());
        Assertions.assertEquals(titleComplement, target.getTitleComplement());
        Assertions.assertEquals(sectionTitle, target.getSectionTitle());
        Assertions.assertEquals(keyTitleQualifier, target.getKeyTitleQualifer());
        Assertions.assertEquals("fre", target.getLanguage());
        Assertions.assertEquals("FR", target.getCountry());
        Assertions.assertEquals(Integer.valueOf(1), target.getNbPcp());
        Assertions.assertEquals(1, target.getPcpList().size());
        Assertions.assertEquals("PCMed", target.getPcpList().stream().findFirst().orElse(null));
        Assertions.assertEquals(2, target.getItems().size());
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

    @Test
    @DisplayName("Test génération statut bibliothèque orphelin")
    void testStatutBibliothequeOrphelin() throws IOException {
        String xml = IOUtils.toString(new FileInputStream(xmlFileOrphan.getFile()), StandardCharsets.UTF_8);

        JacksonXmlModule module = new JacksonXmlModule();
        module.setDefaultUseWrapper(false);
        XmlMapper xmlMapper = new XmlMapper(module);
        NoticeXml notice = xmlMapper.readValue(xml, NoticeXml.class);

        NoticeSolr noticeSolr = mapper.map(notice, NoticeSolr.class);
        Assertions.assertEquals(1, noticeSolr.getStatutList().size());
        Assertions.assertEquals("Orphelin", noticeSolr.getStatutList().stream().findFirst().get());
    }

    @Test
    @DisplayName("Test génération statut bibliothèque PC")
    void testStatutBibliothequePC() throws IOException {
        String xml = IOUtils.toString(new FileInputStream(xmlFilePC.getFile()), StandardCharsets.UTF_8);

        JacksonXmlModule module = new JacksonXmlModule();
        module.setDefaultUseWrapper(false);
        XmlMapper xmlMapper = new XmlMapper(module);
        NoticeXml notice = xmlMapper.readValue(xml, NoticeXml.class);

        NoticeSolr noticeSolr = mapper.map(notice, NoticeSolr.class);
        Assertions.assertEquals(1, noticeSolr.getStatutList().size());
        Assertions.assertEquals("PC", noticeSolr.getStatutList().stream().findFirst().get());
    }

    @Test
    @DisplayName("test construction date de début et date de fin")
    public void buildStartPublicationYearTest() {
        String value = "        f    1975";
        Assertions.assertEquals(mapper.buildStartPublicationYear(value).getYear(), "1975");
        Assertions.assertEquals(mapper.buildStartPublicationYear(value).getConfidenceIndex(), Integer.valueOf(0));
        value = "        f19941995";
        Assertions.assertEquals(mapper.buildStartPublicationYear(value).getYear(), "1994");
        Assertions.assertEquals(mapper.buildStartPublicationYear(value).getConfidenceIndex(), Integer.valueOf(1));
        value = "        f17801789";
        Assertions.assertEquals(mapper.buildStartPublicationYear(value).getYear(), "1780");
        Assertions.assertEquals(mapper.buildStartPublicationYear(value).getConfidenceIndex(), Integer.valueOf(9));
        value = "        b20002010";
        Assertions.assertEquals(mapper.buildStartPublicationYear(value).getYear(), "2000");
        Assertions.assertEquals(mapper.buildStartPublicationYear(value).getConfidenceIndex(), Integer.valueOf(0));
        Assertions.assertEquals(mapper.buildEndPublicationYear(value).getYear(), "2010");
        Assertions.assertEquals(mapper.buildEndPublicationYear(value).getConfidenceIndex(), Integer.valueOf(0));
        value = "        b200     ";
        Assertions.assertEquals(mapper.buildStartPublicationYear(value).getYear(), "200X");
        Assertions.assertEquals(mapper.buildStartPublicationYear(value).getConfidenceIndex(), Integer.valueOf(10));
        Assertions.assertEquals(mapper.buildEndPublicationYear(value).getYear(), null);
        Assertions.assertEquals(mapper.buildEndPublicationYear(value).getConfidenceIndex(), Integer.valueOf(0));
        value = "        a200 9999";
        Assertions.assertEquals(mapper.buildStartPublicationYear(value).getYear(), "200X");
        Assertions.assertEquals(mapper.buildStartPublicationYear(value).getConfidenceIndex(), Integer.valueOf(10));
        Assertions.assertEquals(mapper.buildEndPublicationYear(value).getYear(), null);
        Assertions.assertEquals(mapper.buildEndPublicationYear(value).getConfidenceIndex(), Integer.valueOf(0));
        value = "        a200 1234";
        Assertions.assertEquals(mapper.buildStartPublicationYear(value).getYear(), "200X");
        Assertions.assertEquals(mapper.buildStartPublicationYear(value).getConfidenceIndex(), Integer.valueOf(10));
        String finalValue = value;
        Assertions.assertThrows(IllegalPublicationYearException.class, () -> {mapper.buildEndPublicationYear(finalValue).getYear();});

    }

    @Test
    void testErreurNbLocs() throws IOException {
        String xml = IOUtils.toString(new FileInputStream(xmlFile2.getFile()), StandardCharsets.UTF_8);

        JacksonXmlModule module = new JacksonXmlModule();
        module.setDefaultUseWrapper(false);
        XmlMapper xmlMapper = new XmlMapper(module);
        NoticeXml notice = xmlMapper.readValue(xml, NoticeXml.class);

        NoticeSolr noticeSolr = mapper.map(notice, NoticeSolr.class);

        Assertions.assertEquals(1, noticeSolr.getNbLocation().intValue());


    }
}
