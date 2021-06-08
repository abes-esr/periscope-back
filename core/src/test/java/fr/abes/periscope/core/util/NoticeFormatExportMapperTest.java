package fr.abes.periscope.core.util;

import com.fasterxml.jackson.dataformat.xml.JacksonXmlModule;
import com.fasterxml.jackson.dataformat.xml.XmlMapper;
import fr.abes.periscope.core.entity.visualisation.*;
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

@SpringBootTest(classes = NoticeFormatExportMapper.class)
@ComponentScan(excludeFilters = @ComponentScan.Filter(BaseXMLConfiguration.class))
public class NoticeFormatExportMapperTest {
    @Autowired
    private NoticeFormatExportMapper noticeFormatExportmodelMapper;

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
        Holding hold = new Holding();

        Sequence sequence = noticeFormatExportmodelMapper.genererEtatCollection(hold, notice.getDataFields().get(0));

        Assertions.assertEquals(2000, sequence.getStartDate().get(Calendar.YEAR));
        Assertions.assertEquals(Calendar.JANUARY, sequence.getStartDate().get(Calendar.MONTH));
        Assertions.assertEquals(28, sequence.getStartDate().get(Calendar.DAY_OF_MONTH));
        Assertions.assertEquals("23", sequence.getStartVolume());
        Assertions.assertEquals("38", sequence.getStartNumero());
        Assertions.assertEquals(2000, sequence.getEndDate().get(Calendar.YEAR));
        Assertions.assertEquals(Calendar.DECEMBER, sequence.getEndDate().get(Calendar.MONTH));
        Assertions.assertEquals(31, sequence.getEndDate().get(Calendar.DAY_OF_MONTH));
    }

    @Test
    @DisplayName("test de construction des états de collection / 2 séquences début + fin")
    void buildEtatCollection2() throws IOException {
        String xml = IOUtils.toString(new FileInputStream(xmlFileEtatColl2.getFile()), StandardCharsets.UTF_8);

        JacksonXmlModule module = new JacksonXmlModule();
        module.setDefaultUseWrapper(false);
        XmlMapper xmlMapper = new XmlMapper(module);
        NoticeXml notice = xmlMapper.readValue(xml, NoticeXml.class);
        Holding hold = new Holding();

        Sequence sequence = noticeFormatExportmodelMapper.genererEtatCollection(hold, notice.getDataFields().get(0));

        Calendar calendar = new GregorianCalendar(2000, Calendar.JANUARY, 28);
        Assertions.assertEquals(sdf.format(calendar.getTime()), sdf.format(sequence.getStartDate().getTime()));
        calendar = new GregorianCalendar(2017, Calendar.FEBRUARY, 28);
        Assertions.assertEquals(sdf.format(calendar.getTime()), sdf.format(sequence.getEndDate().getTime()));

        Assertions.assertEquals("23", sequence.getStartVolume());
        Assertions.assertEquals("38", sequence.getStartNumero());
        Assertions.assertEquals("46", sequence.getEndVolume());
        Assertions.assertEquals("42", sequence.getEndNumero());
    }

    @Test
    @DisplayName("test de construction des états de collection / 2 séquences début + fin + 1 numérotation parallèle")
    void buildEtatCollection3() throws IOException {
        String xml = IOUtils.toString(new FileInputStream(xmlFileEtatColl3.getFile()), StandardCharsets.UTF_8);

        JacksonXmlModule module = new JacksonXmlModule();
        module.setDefaultUseWrapper(false);
        XmlMapper xmlMapper = new XmlMapper(module);
        NoticeXml notice = xmlMapper.readValue(xml, NoticeXml.class);
        Holding hold = new Holding();

        Sequence sequence = noticeFormatExportmodelMapper.genererEtatCollection(hold, notice.getDataFields().get(0));

        Calendar calendar = new GregorianCalendar(2000, Calendar.JANUARY, 28);
        Assertions.assertEquals(sdf.format(calendar.getTime()), sdf.format(sequence.getStartDate().getTime()));
        calendar = new GregorianCalendar(2017, Calendar.MARCH, 31);
        Assertions.assertEquals(sdf.format(calendar.getTime()), sdf.format(sequence.getEndDate().getTime()));

        Assertions.assertEquals("23", sequence.getStartVolume());
        Assertions.assertEquals("38", sequence.getStartNumero());
        Assertions.assertEquals("46", sequence.getEndVolume());
        Assertions.assertEquals("42", sequence.getEndNumero());

    }

    @Test
    @DisplayName("test de construction des états de collection / 1 seule séquence de début sans toutes les informations")
    void buildEtatCollection4() throws IOException {
        String xml = IOUtils.toString(new FileInputStream(xmlFileEtatColl4.getFile()), StandardCharsets.UTF_8);

        JacksonXmlModule module = new JacksonXmlModule();
        module.setDefaultUseWrapper(false);
        XmlMapper xmlMapper = new XmlMapper(module);
        NoticeXml notice = xmlMapper.readValue(xml, NoticeXml.class);
        Holding hold = new Holding();

        Sequence sequence = noticeFormatExportmodelMapper.genererEtatCollection(hold, notice.getDataFields().get(0));

        Calendar calendar = new GregorianCalendar(2000, Calendar.JANUARY, 1);
        Assertions.assertEquals(sdf.format(calendar.getTime()), sdf.format(sequence.getStartDate().getTime()));
        Assertions.assertNull(sequence.getEndDate());
    }

    @Test
    @DisplayName("test de construction des états de collection / uniquement informations générales")
    void buildEtatCollection5() throws IOException {
        String xml = IOUtils.toString(new FileInputStream(xmlFileEtatColl5.getFile()), StandardCharsets.UTF_8);

        JacksonXmlModule module = new JacksonXmlModule();
        module.setDefaultUseWrapper(false);
        XmlMapper xmlMapper = new XmlMapper(module);
        NoticeXml notice = xmlMapper.readValue(xml, NoticeXml.class);
        Holding hold = new Holding();

        SequenceContinue sequence = noticeFormatExportmodelMapper.genererEtatCollection(hold, notice.getDataFields().get(0));

        Assertions.assertEquals("vol. 23 no. 38 (28-jan-2000)", hold.getTextEtatCollection());
        Assertions.assertEquals("Lacune", hold.getMentionDeLacune());
        Assertions.assertEquals("commentaire ensemble", hold.getNote());
    }

    @Test
    @DisplayName("test de présence d'erreur dans état de collection source")
    void testErreurEtatCollection() throws IOException {
        String xml = IOUtils.toString(new FileInputStream(xmlFileErreurs.getFile()), StandardCharsets.UTF_8);

        JacksonXmlModule module = new JacksonXmlModule();
        module.setDefaultUseWrapper(false);
        XmlMapper xmlMapper = new XmlMapper(module);
        NoticeXml notice = xmlMapper.readValue(xml, NoticeXml.class);
        Holding hold = new Holding();

        assertThrows(IllegalHoldingException.class, () -> noticeFormatExportmodelMapper.genererEtatCollection(hold, notice.getDataFields().get(0)));
    }

    @Test
    @DisplayName("test de construction des lacunes")
    void buildLacunes() throws IOException {
        String xml = IOUtils.toString(new FileInputStream(xmlFileLacunes.getFile()), StandardCharsets.UTF_8);

        JacksonXmlModule module = new JacksonXmlModule();
        module.setDefaultUseWrapper(false);
        XmlMapper xmlMapper = new XmlMapper(module);
        NoticeXml notice = xmlMapper.readValue(xml, NoticeXml.class);
        Holding hold = new Holding();

        noticeFormatExportmodelMapper.genererLacunes(hold, notice.getDataFields().get(0));

        Assertions.assertTrue(hold.getTextLacune().contains("no.101 (1949 )  ; no.1620 (1979)  ; no.1937 (1985)  ; no.2331 (1993)"));
        Assertions.assertEquals(hold.getSequences().size(), 12);
        Calendar calendar = new GregorianCalendar(1949, Calendar.JANUARY, 1);
        Assertions.assertEquals(sdf.format(calendar.getTime()), sdf.format(hold.getSequences().get(11).getStartDate().getTime()));
        calendar = new GregorianCalendar(2015, Calendar.JANUARY, 1);
        Assertions.assertEquals(sdf.format(calendar.getTime()), sdf.format(hold.getSequences().get(0).getStartDate().getTime()));
        Assertions.assertEquals("101", hold.getSequences().get(11).getStartNumero());
        Assertions.assertEquals("31105", hold.getSequences().get(0).getStartNumero());
    }

    @Test
    @DisplayName("test mapper notice entière")
    void buildNotice() throws IOException {
        String xml = IOUtils.toString(new FileInputStream(xmlFileNotice.getFile()), StandardCharsets.UTF_8);

        JacksonXmlModule module = new JacksonXmlModule();
        module.setDefaultUseWrapper(false);
        XmlMapper xmlMapper = new XmlMapper(module);
        NoticeXml notice = xmlMapper.readValue(xml, NoticeXml.class);

        NoticeVisu noticeVisu = noticeFormatExportmodelMapper.map(notice, NoticeVisu.class);

        Assertions.assertEquals("13282261X", noticeVisu.getPpn());
        Assertions.assertEquals(5, noticeVisu.getHoldings().size());
        Assertions.assertEquals(9, noticeVisu.getHoldings().stream().filter(h -> h.getEpn().equalsIgnoreCase("363392556")).findFirst().get().getSequences().size());
        Assertions.assertEquals(4, noticeVisu.getHoldings().stream().filter(h -> h.getEpn().equalsIgnoreCase("431295026")).findFirst().get().getSequences().size());
        Assertions.assertEquals("no.1987, 3000 (2000) ; no.1000 (2010)", noticeVisu.getHoldings().stream().filter(h -> h.getEpn().equalsIgnoreCase("431295026")).findFirst().get().getTextLacune());
        Assertions.assertEquals(1, noticeVisu.getHoldings().stream().filter(h -> h.getEpn().equalsIgnoreCase("41133793901")).findFirst().get().getSequences().size());
        Assertions.assertEquals(1, noticeVisu.getHoldings().stream().filter(h -> h.getEpn().equalsIgnoreCase("46868145001")).findFirst().get().getSequences().size());
        Assertions.assertEquals(1, noticeVisu.getHoldings().stream().filter(h -> h.getEpn().equalsIgnoreCase("51274287101")).findFirst().get().getSequences().size());

    }


}
