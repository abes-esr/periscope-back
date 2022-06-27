package fr.abes.periscope.web.util;

import fr.abes.periscope.core.entity.solr.*;
import fr.abes.periscope.core.entity.visualisation.Frequency;
import fr.abes.periscope.core.entity.visualisation.NoticeVisu;
import fr.abes.periscope.core.util.UtilsMapper;
import fr.abes.periscope.web.dto.NoticeInfoWebDto;
import fr.abes.periscope.web.dto.NoticeWebV2Dto;
import fr.abes.periscope.web.dto.ResultWebDto;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

@ExtendWith(SpringExtension.class)
@SpringBootTest(classes = {ModelMapper.class, DtoMapper.class, UtilsMapper.class})
public class DtoMapperTest {
    @Autowired
    private UtilsMapper mapper;

    private Notice notice1;
    private Notice notice2;

    @BeforeEach
    void init() {
        notice1 = new Notice();
        notice1.setPpn("111111111");
        notice1.setIssn("1111-1111");
        notice1.setPublisher("test éditeur");
        notice1.setKeyShortedTitle("test keyShortedTitle");
        notice1.setKeyTitleQualifer("test keyTitleQualifier");
        notice1.setParallelTitle("test parallel title");
        notice1.setProperTitle("test proper title");
        notice1.setSectionTitle("test section title");
        notice1.setTitleComplement("test title complement");
        notice1.setTitleFromDifferentAuthor("test title from different author");
        notice1.setKeyTitle("test keyTitle");
        Set<String> listPcp = new HashSet<>();
        listPcp.add("PCMed");
        notice1.setPcpList(listPcp);
        notice1.setContinuousType(OnGoingResourceType.C);
        notice1.setSupportType("test support");
        notice1.setCountry("FR");
        notice1.setLanguage("fre");
        notice1.setStartYear(new PublicationYear("2010", 0));
        notice1.setEndYear(new PublicationYear("2020", 0));
        notice1.setNbLocation(1);

        Item item = new Item();
        item.setPpn("111111111");
        item.setEpn("999999999");
        item.setRcr("341725201");

        notice1.addItem(item);

        Item item2 = new Item();
        item2.setPpn("111111111");
        item2.setEpn("888888888");
        item2.setRcr("111111111");

        notice1.addItem(item2);

        notice2 = new Notice();
        notice2.setPpn("222222222");
        notice2.setIssn("2222-2222");
        notice2.setPublisher("test éditeur 2");
        notice2.setKeyShortedTitle("test keyShortedTitle 2");
        notice2.setKeyTitleQualifer("test keyTitleQualifier 2");
        notice2.setParallelTitle("test parallel title 2");
        notice2.setProperTitle("test proper title 2");
        notice2.setSectionTitle("test section title 2");
        notice2.setTitleComplement("test title complement 2");
        notice2.setTitleFromDifferentAuthor("test title from different author 2");
        notice2.setKeyTitle("test keyTitle 2");
        listPcp = new HashSet<>();
        listPcp.add("PCAq");
        notice2.setPcpList(listPcp);
        notice2.setContinuousType(OnGoingResourceType.E);
        notice2.setSupportType("test support 2");
        notice2.setCountry("US");
        notice2.setLanguage("eng");
        notice2.setStartYear(new PublicationYear("2009", 0));
        notice2.setEndYear(new PublicationYear("2019", 0));
        notice2.setNbLocation(1);
    }

    @Test
    @DisplayName("test converterNotice")
    void converterNoticeTest() {
        NoticeWebV2Dto noticeWebV2Dto = mapper.map(notice1, NoticeWebV2Dto.class);

        Assertions.assertEquals("111111111", noticeWebV2Dto.getPpn());
        Assertions.assertEquals("1111-1111", noticeWebV2Dto.getIssn());
        Assertions.assertEquals("test éditeur", noticeWebV2Dto.getEditeur());
        Assertions.assertEquals("https://www-test.sudoc.fr/111111111", noticeWebV2Dto.getSudocURL());
        Assertions.assertEquals("test keyTitle test keyTitleQualifier", noticeWebV2Dto.getTitre());
        Assertions.assertEquals(Integer.valueOf(1), noticeWebV2Dto.getNbLocation());
        Assertions.assertEquals(1, noticeWebV2Dto.getPcpList().size());
        Assertions.assertEquals("PCMed", noticeWebV2Dto.getPcpList().stream().findFirst().get());
        Assertions.assertEquals(2, noticeWebV2Dto.getRcrList().size());
        Assertions.assertEquals("111111111", noticeWebV2Dto.getRcrList().stream().findFirst().get());

        notice1.setNbLocation(0);
        noticeWebV2Dto = mapper.map(notice1, NoticeWebV2Dto.class);

        Assertions.assertEquals(Integer.valueOf(0), noticeWebV2Dto.getNbLocation());
        Assertions.assertNull(noticeWebV2Dto.getSudocURL());
    }

    @Test
    @DisplayName("test converterResultWebDto")
    void converterResultWebDtoTest() {

        FacetteSolr facette1 = new FacetteSolr(NoticeSolrField.DOCUMENT_TYPE);
        Map<String, Integer> mapFacette = new HashMap<>();
        mapFacette.put("Collection", 10);
        facette1.addValeurs(mapFacette);
        FacetteSolr facette2 = new FacetteSolr(NoticeSolrField.LANGUAGE);
        mapFacette = new HashMap<>();
        mapFacette.put("FR", 12);
        facette1.addValeurs(mapFacette);

        ResultSolr source = new ResultSolr();
        source.addNotice(notice1);
        source.addNotice(notice2);
        source.addFacette(facette1);
        source.addFacette(facette2);
        source.setNbNotices(2);
        source.setNbPages(1);

        ResultWebDto result = mapper.map(source, ResultWebDto.class);

        Assertions.assertEquals(2, result.getNbNotices());
        Assertions.assertEquals(1, result.getNbPages());
        Assertions.assertEquals("111111111", result.getNotices().get(0).getPpn());
        Assertions.assertEquals(2, result.getNbNotices());
        Assertions.assertEquals(2, result.getNbNotices());
        Assertions.assertEquals(2, result.getNbNotices());

    }

    @Test
    @DisplayName("test convert notice web info dot")
    void converterNoticeInfoWebDto() {
        NoticeVisu noticeVisu = new NoticeVisu();
        noticeVisu.setFrequency(Frequency.B);
        noticeVisu.setPpn("111111111");
        noticeVisu.setIssn("1111-1111");
        noticeVisu.setStartYear(new PublicationYear("2020",1));
        noticeVisu.setEndYear(new PublicationYear("2022",1));
        noticeVisu.setPublisher("TestEditor");
        noticeVisu.setKeyTitle("titre clé");
        noticeVisu.setCity("Paris");
        noticeVisu.setSupportType(SupportType.X);
        NoticeInfoWebDto result = mapper.map(noticeVisu, NoticeInfoWebDto.class);

        Assertions.assertEquals("111111111", result.getPpn());
        Assertions.assertEquals("1111-1111", result.getIssn());
        Assertions.assertEquals("(2020)-(2022)", result.getDatePublication());
        Assertions.assertEquals(Frequency.B, result.getPeriodicite());
        Assertions.assertEquals("titre clé", result.getTitre());
        Assertions.assertEquals("Paris", result.getVille());
        Assertions.assertEquals("TestEditor", result.getEditeur());
        Assertions.assertEquals(SupportType.X, result.getTypeSupport());
        noticeVisu.setEndYear(new PublicationYear(null,0));
        result = mapper.map(noticeVisu, NoticeInfoWebDto.class);
        Assertions.assertEquals("(2020)-...", result.getDatePublication());
    }

}
