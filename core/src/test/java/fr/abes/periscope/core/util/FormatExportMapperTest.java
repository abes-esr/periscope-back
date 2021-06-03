package fr.abes.periscope.core.util;

import com.fasterxml.jackson.dataformat.xml.JacksonXmlModule;
import com.fasterxml.jackson.dataformat.xml.XmlMapper;
import fr.abes.periscope.core.entity.visualisation.Lacune;
import fr.abes.periscope.core.entity.visualisation.NoticeVisu;
import fr.abes.periscope.core.entity.visualisation.Sequence;
import fr.abes.periscope.core.entity.xml.NoticeXml;
import fr.abes.periscope.core.exception.IllegalHoldingException;
import org.apache.commons.io.IOUtils;
import org.junit.jupiter.api.Assertions;
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
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.GregorianCalendar;

import static org.junit.jupiter.api.Assertions.assertThrows;

@SpringBootTest(classes = FormatExportMapper.class)
@ComponentScan(excludeFilters = @ComponentScan.Filter(BaseXMLConfiguration.class))
public class FormatExportMapperTest {
    @Autowired
    private FormatExportMapper noticeMapper;

    private final SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");

    @Value("classpath:noticeXml/etatColl1.xml")
    private Resource xmlFileEtatColl1;

    @Value("classpath:noticeXml/etatColl2.xml")
    private Resource xmlFileEtatColl2;

    @Value("classpath:noticeXml/etatColl3.xml")
    private Resource xmlFileEtatColl3;

    @Value("classpath:noticeXml/etatColl4.xml")
    private Resource xmlFileEtatColl4;

    @Value("classpath:noticeXml/etatColl5.xml")
    private Resource xmlFileEtatColl5;

    @Value("classpath:noticeXml/lacunes.xml")
    private Resource xmlFileLacunes;

    @Value("classpath:noticeXml/erreurEtatColl.xml")
    private Resource xmlFileErreurs;

    @Value("classpath:noticeXml/13282261X.xml")
    private Resource xmlFileNotice;

    @Test
    @DisplayName("test de construction des états de collection / 1 seule séquence début")
    void buildEtatCollection1() throws IOException {
        String xml = IOUtils.toString(new FileInputStream(xmlFileEtatColl1.getFile()), StandardCharsets.UTF_8);

        JacksonXmlModule module = new JacksonXmlModule();
        module.setDefaultUseWrapper(false);
        XmlMapper xmlMapper = new XmlMapper(module);
        NoticeXml notice = xmlMapper.readValue(xml, NoticeXml.class);

        Sequence sequence = noticeMapper.genererEtatCollection(notice.getDataFields().get(0));

        Assertions.assertEquals(2000, sequence.getBlocDebut().getDate().get(Calendar.YEAR));
        Assertions.assertEquals(Calendar.JANUARY, sequence.getBlocDebut().getDate().get(Calendar.MONTH));
        Assertions.assertEquals(28, sequence.getBlocDebut().getDate().get(Calendar.DAY_OF_MONTH));
        Assertions.assertEquals("23",sequence.getBlocDebut().getVolume());
        Assertions.assertEquals("38", sequence.getBlocDebut().getNumero());
        Assertions.assertEquals(2000, sequence.getBlocFin().getDate().get(Calendar.YEAR));
        Assertions.assertEquals(Calendar.DECEMBER, sequence.getBlocFin().getDate().get(Calendar.MONTH));
        Assertions.assertEquals(31, sequence.getBlocFin().getDate().get(Calendar.DAY_OF_MONTH));
    }

    @Test
    @DisplayName("test de construction des états de collection / 2 séquences début + fin")
    void buildEtatCollection2() throws IOException {
        String xml = IOUtils.toString(new FileInputStream(xmlFileEtatColl2.getFile()), StandardCharsets.UTF_8);

        JacksonXmlModule module = new JacksonXmlModule();
        module.setDefaultUseWrapper(false);
        XmlMapper xmlMapper = new XmlMapper(module);
        NoticeXml notice = xmlMapper.readValue(xml, NoticeXml.class);

        Sequence sequence = noticeMapper.genererEtatCollection(notice.getDataFields().get(0));

        Calendar calendar = new GregorianCalendar(2000, Calendar.JANUARY, 28);
        Assertions.assertEquals(sdf.format(calendar.getTime()), sdf.format(sequence.getBlocDebut().getDate().getTime()));
        calendar =  new GregorianCalendar(2017, Calendar.FEBRUARY, 28);
        Assertions.assertEquals(sdf.format(calendar.getTime()), sdf.format(sequence.getBlocFin().getDate().getTime()));

        Assertions.assertEquals("23", sequence.getBlocDebut().getVolume());
        Assertions.assertEquals("38", sequence.getBlocDebut().getNumero());
        Assertions.assertEquals("46", sequence.getBlocFin().getVolume());
        Assertions.assertEquals("42", sequence.getBlocFin().getNumero());
    }

    @Test
    @DisplayName("test de construction des états de collection / 2 séquences début + fin + 1 numérotation parallèle")
    void buildEtatCollection3() throws IOException {
        String xml = IOUtils.toString(new FileInputStream(xmlFileEtatColl3.getFile()), StandardCharsets.UTF_8);

        JacksonXmlModule module = new JacksonXmlModule();
        module.setDefaultUseWrapper(false);
        XmlMapper xmlMapper = new XmlMapper(module);
        NoticeXml notice = xmlMapper.readValue(xml, NoticeXml.class);

        Sequence sequence = noticeMapper.genererEtatCollection(notice.getDataFields().get(0));

        Calendar calendar = new GregorianCalendar(2000, Calendar.JANUARY, 28);
        Assertions.assertEquals(sdf.format(calendar.getTime()), sdf.format(sequence.getBlocDebut().getDate().getTime()));
        calendar =  new GregorianCalendar(2017, Calendar.MARCH, 31);
        Assertions.assertEquals(sdf.format(calendar.getTime()), sdf.format(sequence.getBlocFin().getDate().getTime()));

        Assertions.assertEquals("23", sequence.getBlocDebut().getVolume());
        Assertions.assertEquals("38", sequence.getBlocDebut().getNumero());
        Assertions.assertEquals("46", sequence.getBlocFin().getVolume());
        Assertions.assertEquals("42", sequence.getBlocFin().getNumero());

    }

    @Test
    @DisplayName("test de construction des états de collection / 1 seule séquence de début sans toutes les informations")
    void buildEtatCollection4() throws IOException {
        String xml = IOUtils.toString(new FileInputStream(xmlFileEtatColl4.getFile()), StandardCharsets.UTF_8);

        JacksonXmlModule module = new JacksonXmlModule();
        module.setDefaultUseWrapper(false);
        XmlMapper xmlMapper = new XmlMapper(module);
        NoticeXml notice = xmlMapper.readValue(xml, NoticeXml.class);

        Sequence sequence = noticeMapper.genererEtatCollection(notice.getDataFields().get(0));

        Calendar calendar = new GregorianCalendar(2000, Calendar.JANUARY, 1);
        Assertions.assertEquals(sdf.format(calendar.getTime()), sdf.format(sequence.getBlocDebut().getDate().getTime()));
        Assertions.assertNull(sequence.getBlocFin());
    }

    @Test
    @DisplayName("test de construction des états de collection / uniquement informations générales")
    void buildEtatCollection5() throws IOException {
        String xml = IOUtils.toString(new FileInputStream(xmlFileEtatColl5.getFile()), StandardCharsets.UTF_8);

        JacksonXmlModule module = new JacksonXmlModule();
        module.setDefaultUseWrapper(false);
        XmlMapper xmlMapper = new XmlMapper(module);
        NoticeXml notice = xmlMapper.readValue(xml, NoticeXml.class);

        Sequence sequence = noticeMapper.genererEtatCollection(notice.getDataFields().get(0));

        Assertions.assertEquals("vol. 23 no. 38 (28-jan-2000)", sequence.getTexteEtatCollectionZone());
        Assertions.assertEquals("Lacune", sequence.getMentionDeLacune());
        Assertions.assertEquals("commentaire ensemble", sequence.getNote());
    }

    @Test
    @DisplayName("test de présence d'erreur dans état de collection source")
    void testErreurEtatCollection() throws IOException {
        String xml = IOUtils.toString(new FileInputStream(xmlFileErreurs.getFile()), StandardCharsets.UTF_8);

        JacksonXmlModule module = new JacksonXmlModule();
        module.setDefaultUseWrapper(false);
        XmlMapper xmlMapper = new XmlMapper(module);
        NoticeXml notice = xmlMapper.readValue(xml, NoticeXml.class);

        assertThrows(IllegalHoldingException.class, () -> noticeMapper.genererEtatCollection(notice.getDataFields().get(0)));
    }

    @Test
    @DisplayName("test de construction des lacunes")
    void buildLacunes() throws IOException {
        String xml = IOUtils.toString(new FileInputStream(xmlFileLacunes.getFile()), StandardCharsets.UTF_8);

        JacksonXmlModule module = new JacksonXmlModule();
        module.setDefaultUseWrapper(false);
        XmlMapper xmlMapper = new XmlMapper(module);
        NoticeXml notice = xmlMapper.readValue(xml, NoticeXml.class);

        Lacune lacune = noticeMapper.genererLacunes(notice.getDataFields().get(0));

        Assertions.assertTrue(lacune.getCommentaire().contains("no.101 (1949 )  ; no.1620 (1979)  ; no.1937 (1985)  ; no.2331 (1993)"));
        Assertions.assertEquals(lacune.getBlocs().size(), 12);
        Calendar calendar = new GregorianCalendar(1949, Calendar.JANUARY, 1);
        Assertions.assertEquals(sdf.format(calendar.getTime()), sdf.format(lacune.getBlocs().get(0).getDate().getTime()));
        calendar =  new GregorianCalendar(2015, Calendar.JANUARY, 1);
        Assertions.assertEquals(sdf.format(calendar.getTime()), sdf.format(lacune.getBlocs().get(11).getDate().getTime()));
        Assertions.assertEquals("101", lacune.getBlocs().get(0).getNumero());
        Assertions.assertEquals("31105", lacune.getBlocs().get(11).getNumero());
    }

    @Test
    @DisplayName("test mapper notice entière")
    void buildNotice() throws IOException {
        String xml = IOUtils.toString(new FileInputStream(xmlFileNotice.getFile()), StandardCharsets.UTF_8);

        JacksonXmlModule module = new JacksonXmlModule();
        module.setDefaultUseWrapper(false);
        XmlMapper xmlMapper = new XmlMapper(module);
        NoticeXml notice = xmlMapper.readValue(xml, NoticeXml.class);

        NoticeVisu noticeVisu = noticeMapper.map(notice, NoticeVisu.class);

        Assertions.assertEquals("13282261X", noticeVisu.getPpn());
        Assertions.assertEquals(5, noticeVisu.getHoldings().size());
        Assertions.assertEquals(5, noticeVisu.getHoldings().get(0).getSequences().size());

    }


}
