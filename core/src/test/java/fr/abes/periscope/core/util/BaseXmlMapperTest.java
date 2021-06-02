package fr.abes.periscope.core.util;

import com.fasterxml.jackson.dataformat.xml.JacksonXmlModule;
import com.fasterxml.jackson.dataformat.xml.XmlMapper;
import fr.abes.periscope.core.entity.EnumMonth;
import fr.abes.periscope.core.entity.visualisation.Lacune;
import fr.abes.periscope.core.entity.visualisation.Sequence;
import fr.abes.periscope.core.entity.xml.NoticeXml;
import fr.abes.periscope.core.exception.IllegalHoldingException;
import org.apache.commons.io.IOUtils;
import org.junit.Assert;
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

import static org.junit.jupiter.api.Assertions.assertThrows;

@SpringBootTest(classes = BaseXmlMapper.class)
@ComponentScan(excludeFilters = @ComponentScan.Filter(BaseXMLConfiguration.class))
public class BaseXmlMapperTest {
    @Autowired
    private BaseXmlMapper noticeMapper;
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

    @Test
    @DisplayName("test de construction des états de collection / 1 seule séquence début")
    public void buildEtatCollection1() throws IOException {
        String xml = IOUtils.toString(new FileInputStream(xmlFileEtatColl1.getFile()), StandardCharsets.UTF_8);

        JacksonXmlModule module = new JacksonXmlModule();
        module.setDefaultUseWrapper(false);
        XmlMapper xmlMapper = new XmlMapper(module);
        NoticeXml notice = xmlMapper.readValue(xml, NoticeXml.class);

        Sequence sequence = noticeMapper.genererEtatCollection(notice.getDataFields().get(0));

        Assert.assertEquals((long)sequence.getBlocDebut().getAnnee(), 2000);
        Assert.assertEquals(sequence.getBlocDebut().getMois(), EnumMonth.jan);
        Assert.assertEquals((long)sequence.getBlocDebut().getJour(), 28);
        Assert.assertEquals(sequence.getBlocDebut().getVolume(), "23");
        Assert.assertEquals(sequence.getBlocDebut().getNumero(), "38");
    }

    @Test
    @DisplayName("test de construction des états de collection / 2 séquences début + fin")
    public void buildEtatCollection2() throws IOException {
        String xml = IOUtils.toString(new FileInputStream(xmlFileEtatColl2.getFile()), StandardCharsets.UTF_8);

        JacksonXmlModule module = new JacksonXmlModule();
        module.setDefaultUseWrapper(false);
        XmlMapper xmlMapper = new XmlMapper(module);
        NoticeXml notice = xmlMapper.readValue(xml, NoticeXml.class);

        Sequence sequence = noticeMapper.genererEtatCollection(notice.getDataFields().get(0));

        Assert.assertEquals((long)sequence.getBlocDebut().getAnnee(), 2000);
        Assert.assertEquals(sequence.getBlocDebut().getMois(), EnumMonth.jan);
        Assert.assertEquals((long)sequence.getBlocDebut().getJour(), 28);
        Assert.assertEquals(sequence.getBlocDebut().getVolume(), "23");
        Assert.assertEquals(sequence.getBlocDebut().getNumero(), "38");
        Assert.assertEquals((long)sequence.getBlocFin().getAnnee(), 2017);
        Assert.assertEquals(sequence.getBlocFin().getMois(), EnumMonth.mar);
        Assert.assertEquals((long)sequence.getBlocFin().getJour(), 31);
        Assert.assertEquals(sequence.getBlocFin().getVolume(), "46");
        Assert.assertEquals(sequence.getBlocFin().getNumero(), "42");
    }

    @Test
    @DisplayName("test de construction des états de collection / 2 séquences début + fin + 1 numérotation parallèle")
    public void buildEtatCollection3() throws IOException {
        String xml = IOUtils.toString(new FileInputStream(xmlFileEtatColl3.getFile()), StandardCharsets.UTF_8);

        JacksonXmlModule module = new JacksonXmlModule();
        module.setDefaultUseWrapper(false);
        XmlMapper xmlMapper = new XmlMapper(module);
        NoticeXml notice = xmlMapper.readValue(xml, NoticeXml.class);

        Sequence sequence = noticeMapper.genererEtatCollection(notice.getDataFields().get(0));

        Assert.assertEquals((long)sequence.getBlocDebut().getAnnee(), 2000);
        Assert.assertEquals(sequence.getBlocDebut().getMois(), EnumMonth.jan);
        Assert.assertEquals((long)sequence.getBlocDebut().getJour(), 28);
        Assert.assertEquals(sequence.getBlocDebut().getVolume(), "23");
        Assert.assertEquals(sequence.getBlocDebut().getNumero(), "38");
        Assert.assertEquals((long)sequence.getBlocFin().getAnnee(), 2017);
        Assert.assertEquals(sequence.getBlocFin().getMois(), EnumMonth.mar);
        Assert.assertEquals((long)sequence.getBlocFin().getJour(), 31);
        Assert.assertEquals(sequence.getBlocFin().getVolume(), "46");
        Assert.assertEquals(sequence.getBlocFin().getNumero(), "42");

    }

    @Test
    @DisplayName("test de construction des états de collection / 1 seule séquence de début sans toutes les informations")
    public void buildEtatCollection4() throws IOException {
        String xml = IOUtils.toString(new FileInputStream(xmlFileEtatColl4.getFile()), StandardCharsets.UTF_8);

        JacksonXmlModule module = new JacksonXmlModule();
        module.setDefaultUseWrapper(false);
        XmlMapper xmlMapper = new XmlMapper(module);
        NoticeXml notice = xmlMapper.readValue(xml, NoticeXml.class);

        Sequence sequence = noticeMapper.genererEtatCollection(notice.getDataFields().get(0));

        Assert.assertEquals((long)sequence.getBlocDebut().getAnnee(), 2000);
        Assert.assertEquals(sequence.getBlocDebut().getMois(), EnumMonth.jan);
        Assert.assertEquals((long)sequence.getBlocDebut().getJour(), 28);
    }

    @Test
    @DisplayName("test de construction des états de collection / uniquement informations générales")
    public void buildEtatCollection5() throws IOException {
        String xml = IOUtils.toString(new FileInputStream(xmlFileEtatColl5.getFile()), StandardCharsets.UTF_8);

        JacksonXmlModule module = new JacksonXmlModule();
        module.setDefaultUseWrapper(false);
        XmlMapper xmlMapper = new XmlMapper(module);
        NoticeXml notice = xmlMapper.readValue(xml, NoticeXml.class);

        Sequence sequence = noticeMapper.genererEtatCollection(notice.getDataFields().get(0));

        Assert.assertEquals(sequence.getTexteEtatCollectionZone(), "vol. 23 no. 38 (28-jan-2000)");
        Assert.assertEquals(sequence.getMentionDeLacune(), "Lacune");
        Assert.assertEquals(sequence.getNote(), "commentaire ensemble");
    }

    @Test
    @DisplayName("test de présence d'erreur dans état de collection source")
    public void testErreurEtatCollection() throws IOException {
        String xml = IOUtils.toString(new FileInputStream(xmlFileErreurs.getFile()), StandardCharsets.UTF_8);

        JacksonXmlModule module = new JacksonXmlModule();
        module.setDefaultUseWrapper(false);
        XmlMapper xmlMapper = new XmlMapper(module);
        NoticeXml notice = xmlMapper.readValue(xml, NoticeXml.class);

        assertThrows(IllegalHoldingException.class, () -> noticeMapper.genererEtatCollection(notice.getDataFields().get(0)));
    }

    @Test
    @DisplayName("test de construction des lacunes")
    public void buildLacunes() throws IOException {
        String xml = IOUtils.toString(new FileInputStream(xmlFileLacunes.getFile()), StandardCharsets.UTF_8);

        JacksonXmlModule module = new JacksonXmlModule();
        module.setDefaultUseWrapper(false);
        XmlMapper xmlMapper = new XmlMapper(module);
        NoticeXml notice = xmlMapper.readValue(xml, NoticeXml.class);

        Lacune lacune = noticeMapper.genererLacunes(notice.getDataFields().get(0));

        Assert.assertTrue(lacune.getCommentaire().contains("no.101 (1949 )  ; no.1620 (1979)  ; no.1937 (1985)  ; no.2331 (1993)"));
        Assert.assertEquals(lacune.getBlocs().size(), 12);
        Assert.assertEquals((long)lacune.getBlocs().get(0).getAnnee(), 1949);
        Assert.assertEquals(lacune.getBlocs().get(0).getNumero(), "101");
        Assert.assertEquals((long)lacune.getBlocs().get(11).getAnnee(), 2015);
        Assert.assertEquals(lacune.getBlocs().get(11).getNumero(), "31105");
    }
}
