package fr.abes.periscope.core.util;

import com.fasterxml.jackson.dataformat.xml.JacksonXmlModule;
import com.fasterxml.jackson.dataformat.xml.XmlMapper;
import fr.abes.periscope.core.entity.visualisation.Holding;
import fr.abes.periscope.core.entity.visualisation.NoticeVisu;
import fr.abes.periscope.core.entity.visualisation.SequenceContinue;
import fr.abes.periscope.core.entity.xml.NoticeXml;
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
import java.time.Period;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.Optional;

@SpringBootTest(classes = { NoticeMapper.class, NoticeFormatExportMapper.class})
@ComponentScan(excludeFilters = @ComponentScan.Filter(BaseXMLConfiguration.class))
class NoticeFormatExportMapperTest {
    @Autowired
    private NoticeFormatExportMapper noticeFormatExportmodelMapper;

    @Autowired
    private NoticeMapper mapper;

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

    @Value("classpath:noticeXml/etatColl6.xml")
    private Resource xmlFileEtatColl6;

    @Value("classpath:noticeXml/lacunes.xml")
    private Resource xmlFileLacunes;

    @Value("classpath:noticeXml/erreurEtatColl.xml")
    private Resource xmlFileErreurs;

    @Value("classpath:noticeXml/erreurEtatColl2.xml")
    private Resource xmlFileErreurs2;

    @Value("classpath:noticeXml/13282261X.xml")
    private Resource xmlFileNotice;

    @Value("classpath:noticeXml/039226859.xml")
    private Resource hugeXmlFileNotice;

    @Test
    @DisplayName("test de construction des états de collection / 1 seule séquence début")
    void buildEtatCollection1() throws IOException {
        String xml = IOUtils.toString(new FileInputStream(xmlFileEtatColl1.getFile()), StandardCharsets.UTF_8);

        JacksonXmlModule module = new JacksonXmlModule();
        module.setDefaultUseWrapper(false);
        XmlMapper xmlMapper = new XmlMapper(module);
        NoticeXml notice = xmlMapper.readValue(xml, NoticeXml.class);
        Holding hold = new Holding("41133793901");

        noticeFormatExportmodelMapper.processEtatCollection(hold, notice.getDataFields().get(0));

        Assertions.assertEquals(1, hold.getSequences().size());
        Assertions.assertEquals(2000, Optional.of(hold.getSequences().get(0).getStartDate()).get().intValue());
        Assertions.assertEquals("23", ((SequenceContinue)hold.getSequences().get(0)).getStartVolume());
        Assertions.assertEquals("38", ((SequenceContinue)hold.getSequences().get(0)).getStartNumero());
        //cas d'un intervalle avec date de début sans date de fin mais fermé
        Assertions.assertEquals(2000, Optional.of(hold.getSequences().get(0).getEndDate()).get().intValue());
    }

    @Test
    @DisplayName("test de construction des états de collection / 2 séquences début + fin")
    void buildEtatCollection2() throws IOException {
        String xml = IOUtils.toString(new FileInputStream(xmlFileEtatColl2.getFile()), StandardCharsets.UTF_8);

        JacksonXmlModule module = new JacksonXmlModule();
        module.setDefaultUseWrapper(false);
        XmlMapper xmlMapper = new XmlMapper(module);
        NoticeXml notice = xmlMapper.readValue(xml, NoticeXml.class);
        Holding hold = new Holding("41133793901");

        noticeFormatExportmodelMapper.processEtatCollection(hold, notice.getDataFields().get(0));

        Assertions.assertEquals(1, hold.getSequences().size());

        Assertions.assertEquals(2000, hold.getSequences().get(0).getStartDate().intValue());
        Assertions.assertEquals(2017, hold.getSequences().get(0).getEndDate().intValue());

        Assertions.assertEquals("23", ((SequenceContinue)hold.getSequences().get(0)).getStartVolume());
        Assertions.assertEquals("38", ((SequenceContinue)hold.getSequences().get(0)).getStartNumero());
        Assertions.assertEquals("46", ((SequenceContinue)hold.getSequences().get(0)).getEndVolume());
        Assertions.assertEquals("42", ((SequenceContinue)hold.getSequences().get(0)).getEndNumero());
    }

    @Test
    @DisplayName("test de construction des états de collection / 2 séquences début + fin + 1 numérotation parallèle")
    void buildEtatCollection3() throws IOException {
        String xml = IOUtils.toString(new FileInputStream(xmlFileEtatColl3.getFile()), StandardCharsets.UTF_8);

        JacksonXmlModule module = new JacksonXmlModule();
        module.setDefaultUseWrapper(false);
        XmlMapper xmlMapper = new XmlMapper(module);
        NoticeXml notice = xmlMapper.readValue(xml, NoticeXml.class);
        Holding hold = new Holding("41133793901");

        noticeFormatExportmodelMapper.processEtatCollection(hold, notice.getDataFields().get(0));

        Assertions.assertEquals(1, hold.getSequences().size());

        Assertions.assertEquals(2000, hold.getSequences().get(0).getStartDate().intValue());
        Assertions.assertEquals(2017, hold.getSequences().get(0).getEndDate().intValue());

        Assertions.assertEquals("23", ((SequenceContinue)hold.getSequences().get(0)).getStartVolume());
        Assertions.assertEquals("38", ((SequenceContinue)hold.getSequences().get(0)).getStartNumero());
        Assertions.assertEquals("46", ((SequenceContinue)hold.getSequences().get(0)).getEndVolume());
        Assertions.assertEquals("42", ((SequenceContinue)hold.getSequences().get(0)).getEndNumero());

    }

    @Test
    @DisplayName("test de construction des états de collection / 1 seule séquence de début sans toutes les informations")
    void buildEtatCollection4() throws IOException {
        String xml = IOUtils.toString(new FileInputStream(xmlFileEtatColl4.getFile()), StandardCharsets.UTF_8);

        JacksonXmlModule module = new JacksonXmlModule();
        module.setDefaultUseWrapper(false);
        XmlMapper xmlMapper = new XmlMapper(module);
        NoticeXml notice = xmlMapper.readValue(xml, NoticeXml.class);
        Holding hold = new Holding("41133793901");

        noticeFormatExportmodelMapper.processEtatCollection(hold, notice.getDataFields().get(0));

        Assertions.assertEquals(1, hold.getSequences().size());

        //cas d'un intervalle ouvert, la date de fin doit être égale à la date du jour
        Calendar calendar1 = new GregorianCalendar();
        Assertions.assertEquals(calendar1.get(Calendar.YEAR), hold.getSequences().get(0).getEndDate().intValue());
    }

    @Test
    @DisplayName("test de construction des états de collection / uniquement informations générales")
    void buildEtatCollection5() throws IOException {
        String xml = IOUtils.toString(new FileInputStream(xmlFileEtatColl5.getFile()), StandardCharsets.UTF_8);

        JacksonXmlModule module = new JacksonXmlModule();
        module.setDefaultUseWrapper(false);
        XmlMapper xmlMapper = new XmlMapper(module);
        NoticeXml notice = xmlMapper.readValue(xml, NoticeXml.class);
        Holding hold = new Holding("41133793901");

        noticeFormatExportmodelMapper.processEtatCollection(hold, notice.getDataFields().get(0));

        Assertions.assertEquals(0, hold.getSequences().size());

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
        Holding hold = new Holding("41133793901");

        noticeFormatExportmodelMapper.processEtatCollection(hold, notice.getDataFields().get(0));

        Assertions.assertEquals(1, hold.getErrorSequences().size());
    }

    @Test
    @DisplayName("Test sur erreur dans date de début dans Etat de collection")
    void testErreurEtatCollection2() throws IOException {
        String xml = IOUtils.toString(new FileInputStream(xmlFileErreurs2.getFile()), StandardCharsets.UTF_8);

        JacksonXmlModule module = new JacksonXmlModule();
        module.setDefaultUseWrapper(false);
        XmlMapper xmlMapper = new XmlMapper(module);
        NoticeXml notice = xmlMapper.readValue(xml, NoticeXml.class);
        Holding hold = new Holding("41133793901");
        noticeFormatExportmodelMapper.processEtatCollection(hold, notice.getDataFields().get(0));

        Assertions.assertEquals(1, hold.getErreurs().size());
        Assertions.assertEquals("Erreur epn 41133793901 : syntaxe de date incorrecte : 1946/1947", hold.getErreurs().get(0));
    }

    @Test
    @DisplayName("test de construction des lacunes")
    void buildLacunes() throws IOException {
        String xml = IOUtils.toString(new FileInputStream(xmlFileLacunes.getFile()), StandardCharsets.UTF_8);

        JacksonXmlModule module = new JacksonXmlModule();
        module.setDefaultUseWrapper(false);
        XmlMapper xmlMapper = new XmlMapper(module);
        NoticeXml notice = xmlMapper.readValue(xml, NoticeXml.class);
        Holding hold = new Holding("41133793901");
        SequenceContinue sequence = new SequenceContinue(1948, "","", 2017, "","");
        hold.addSequence(sequence);

        noticeFormatExportmodelMapper.processLacunes(hold, notice.getDataFields().get(0));

        Assertions.assertEquals(19, hold.getSequences().size());

        Assertions.assertTrue(hold.getTextLacune().contains("no.101 (1949 )  ; no.1620 (1979)  ; no.1937 (1985)  ; no.2331 (1993)"));
        Assertions.assertEquals(10, hold.getLacuneSequences().size());
        Assertions.assertEquals(1949, hold.getLacuneSequences().get(0).getStartDate().intValue());
        Assertions.assertEquals("101", hold.getLacuneSequences().get(0).getNumero());
        Assertions.assertEquals(2015, hold.getLacuneSequences().get(9).getStartDate().intValue());
        Assertions.assertEquals("31105",  hold.getLacuneSequences().get(9).getNumero());
    }

    @Test
    @DisplayName("test mapper notice entière")
    void buildNotice() throws IOException {
        String xml = IOUtils.toString(new FileInputStream(xmlFileNotice.getFile()), StandardCharsets.UTF_8);

        JacksonXmlModule module = new JacksonXmlModule();
        module.setDefaultUseWrapper(false);
        XmlMapper xmlMapper = new XmlMapper(module);
        NoticeXml notice = xmlMapper.readValue(xml, NoticeXml.class);

        NoticeVisu noticeVisu = mapper.map(notice, NoticeVisu.class);

        Assertions.assertEquals("13282261X", noticeVisu.getPpn());
        Assertions.assertEquals(6, noticeVisu.getHoldings().size());
        Assertions.assertEquals(4, noticeVisu.getHoldings().stream().filter(h -> h.getEpn().equalsIgnoreCase("363392556")).findFirst().get().getLacuneSequences().size());
        Assertions.assertEquals(1, noticeVisu.getHoldings().stream().filter(h -> h.getEpn().equalsIgnoreCase("363392556")).findFirst().get().getErrorSequences().size());
        Assertions.assertEquals(8, noticeVisu.getHoldings().stream().filter(h -> h.getEpn().equalsIgnoreCase("363392556")).findFirst().get().getContinueSequences().size());

        Assertions.assertEquals(2, noticeVisu.getHoldings().stream().filter(h -> h.getEpn().equalsIgnoreCase("431295026")).findFirst().get().getLacuneSequences().size());
        Assertions.assertEquals(3, noticeVisu.getHoldings().stream().filter(h -> h.getEpn().equalsIgnoreCase("431295026")).findFirst().get().getContinueSequences().size());

        Assertions.assertEquals(1, noticeVisu.getHoldings().stream().filter(h -> h.getEpn().equalsIgnoreCase("41133793901")).findFirst().get().getSequences().size());

        Assertions.assertEquals(1, noticeVisu.getHoldings().stream().filter(h -> h.getEpn().equalsIgnoreCase("46868145001")).findFirst().get().getSequences().size());

        Assertions.assertEquals(1, noticeVisu.getHoldings().stream().filter(h -> h.getEpn().equalsIgnoreCase("51274287101")).findFirst().get().getSequences().size());

    }

    @Test
    @DisplayName("test de construction des états de collection / chevauchement séquence date début + date début sans date de fin")
    void buildEtatCollection6() throws IOException {
        String xml = IOUtils.toString(new FileInputStream(xmlFileEtatColl6.getFile()), StandardCharsets.UTF_8);

        JacksonXmlModule module = new JacksonXmlModule();
        module.setDefaultUseWrapper(false);
        XmlMapper xmlMapper = new XmlMapper(module);
        NoticeXml notice = xmlMapper.readValue(xml, NoticeXml.class);
        NoticeVisu noticeVisu = mapper.map(notice, NoticeVisu.class);

        Assertions.assertEquals(1, noticeVisu.getHoldings().size());
    }

    @Test
    @DisplayName("test méthode récupération fréquence")
    void testExtractFrequency() {
        String frequency = "u";
        Assertions.assertEquals(Period.ZERO, noticeFormatExportmodelMapper.extractFrequency(frequency));
        frequency = "b";
        Assertions.assertEquals(Period.ofDays(3), noticeFormatExportmodelMapper.extractFrequency(frequency));
    }

    @Test
    @DisplayName("test construction énorme notice")
    void testHugeNotice() throws IOException {
        String xml = IOUtils.toString(new FileInputStream(hugeXmlFileNotice.getFile()), StandardCharsets.UTF_8);

        JacksonXmlModule module = new JacksonXmlModule();
        module.setDefaultUseWrapper(false);
        XmlMapper xmlMapper = new XmlMapper(module);
        NoticeXml notice = xmlMapper.readValue(xml, NoticeXml.class);

        NoticeVisu noticeVisu = mapper.map(notice, NoticeVisu.class);

        Assertions.assertEquals("039226859", noticeVisu.getPpn());
    }


}
