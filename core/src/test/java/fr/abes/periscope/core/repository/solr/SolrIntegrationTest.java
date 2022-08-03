package fr.abes.periscope.core.repository.solr;

import com.fasterxml.jackson.dataformat.xml.JacksonXmlModule;
import com.fasterxml.jackson.dataformat.xml.XmlMapper;
import fr.abes.periscope.core.CoreTestConfiguration;
import fr.abes.periscope.core.EnableOnIntegrationTest;
import fr.abes.periscope.core.entity.solr.NoticeSolr;
import fr.abes.periscope.core.entity.xml.NoticeXml;
import fr.abes.periscope.core.service.NoticeStoreService;
import fr.abes.periscope.core.service.NoticesBibioService;
import fr.abes.periscope.core.util.BaseXMLConfiguration;
import fr.abes.periscope.core.util.UtilsMapper;
import org.apache.commons.io.IOUtils;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.core.io.Resource;

import java.io.FileInputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;

import static org.assertj.core.api.Assertions.assertThat;

@EnableOnIntegrationTest
@SpringBootTest(classes = {CoreTestConfiguration.class})
@ComponentScan(excludeFilters = @ComponentScan.Filter(BaseXMLConfiguration.class))
class SolrIntegrationTest {
    @Autowired
    private UtilsMapper noticeMapper;

    @Autowired
    private NoticeStoreService noticeService;

    @Autowired
    private NoticeSolrRepository noticeSolrRepository;

    @Autowired
    private NoticesBibioService service;

    @Value("classpath:noticeXml/123456789.xml")
    private Resource xmlFile1;

    @Value(("classpath:noticeXml/999999999.xml"))
    private Resource xmlFile2;

    private NoticeSolr getNoticeFromFile(Resource file) throws IOException {
        String xml = IOUtils.toString(new FileInputStream(file.getFile()), StandardCharsets.UTF_8);

        JacksonXmlModule module = new JacksonXmlModule();
        module.setDefaultUseWrapper(false);
        XmlMapper xmlMapper = new XmlMapper(module);
        NoticeXml notice = xmlMapper.readValue(xml, NoticeXml.class);
        return noticeMapper.map(notice, NoticeSolr.class);
    }

    @Test
    @DisplayName("ajout d'une notice avec exemplaire")
    public void addNoticeToSolr() throws IOException {
        NoticeSolr notice = getNoticeFromFile(xmlFile1);
        noticeService.save(notice);
    }

    @Test
    @DisplayName("supression d'une notice avec exemplaire")
    public void deleteNoticeToSolr() throws IOException {
        NoticeSolr notice = getNoticeFromFile(xmlFile1);
        noticeService.delete(notice);
    }

    @Test
    @DisplayName("test indexation champs notice")
    public void indexChampsNotice() throws IOException {
        NoticeSolr notice = getNoticeFromFile(xmlFile1);
        noticeService.save(notice);
        NoticeSolr noticesolrOut = noticeService.findByPpn("13282261X");

        System.out.println(noticesolrOut);
        assertThat(noticesolrOut.getIssn()).isEqualTo("2100-1456");
        assertThat(noticesolrOut.getEditorForDisplay()).isEqualTo("Fédération Allier Nature, FAN");
        assertThat(noticesolrOut.getEditor().size()).isEqualTo(3);
        assertThat(noticesolrOut.getKeyTitle()).isEqualTo("Auvergne environnement");
        assertThat(noticesolrOut.getKeyShortedTitle().size()).isEqualTo(2);
        assertThat(noticesolrOut.getProperTitle().size()).isEqualTo(2);
        assertThat(noticesolrOut.getProperTitleForDisplay()).isEqualTo("Auvergne environnement");
        assertThat(noticesolrOut.getTitleFromDifferentAuthor().size()).isEqualTo(2);
        assertThat(noticesolrOut.getTitleFromDifferentAuthorForDisplay()).isEqualTo("Titre from different author");
        assertThat(noticesolrOut.getParallelTitle().size()).isEqualTo(2);
        assertThat(noticesolrOut.getParallelTitleForDisplay()).isEqualTo("Titre parallèle");
        assertThat(noticesolrOut.getTitleComplement().size()).isEqualTo(2);
        assertThat(noticesolrOut.getTitleComplementForDisplay()).isEqualTo("magazine trimestriel des associations pour la nature et l'environnement");
        assertThat(noticesolrOut.getSectionTitle().size()).isEqualTo(2);
        assertThat(noticesolrOut.getSectionTitleForDisplay()).isEqualTo("Titre de section");
        assertThat(noticesolrOut.getKeyTitleQualifer()).isEqualTo("Key title qualifier");
        assertThat(noticesolrOut.getNbLocation()).isEqualTo(5);
        assertThat(noticesolrOut.getLanguageForDisplay()).isEqualTo("fre");
        assertThat(noticesolrOut.getCountryForDisplay()).isEqualTo("FR");
        assertThat(noticesolrOut.getEndYear()).isEqualTo("201X");
        assertThat(noticesolrOut.getEndYearConfidenceIndex()).isEqualTo(10);
        assertThat(noticesolrOut.getStartYear()).isEqualTo("2009");
        assertThat(noticesolrOut.getStartYearConfidenceIndex()).isEqualTo(0);
        assertThat(noticesolrOut.getExternalURLs().size()).isEqualTo(2);
        assertThat(noticesolrOut.getNbPcp()).isEqualTo(2);

        noticeService.delete(notice);
    }

    @Test
    @DisplayName("test indexation PCP")
    public void indexPCP() throws IOException {
        NoticeSolr notice = getNoticeFromFile(xmlFile2);
        noticeService.save(notice);
        NoticeSolr noticesolrOut = noticeService.findByPpn("999999999");

        System.out.println(noticesolrOut);
        assertThat(noticesolrOut.getNbLocation()).isEqualTo(47);
        assertThat(noticesolrOut.getNbPcp()).isEqualTo(2);

        noticeService.delete(notice);
    }
}
