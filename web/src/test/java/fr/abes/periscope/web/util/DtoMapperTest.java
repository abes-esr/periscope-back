package fr.abes.periscope.web.util;

import fr.abes.periscope.core.entity.solr.Item;
import fr.abes.periscope.core.entity.solr.OnGoingResourceType;
import fr.abes.periscope.core.entity.solr.PublicationYear;
import fr.abes.periscope.core.entity.solr.v2.NoticeV2;
import fr.abes.periscope.core.entity.solr.v2.FacetteSolr;
import fr.abes.periscope.core.entity.solr.v2.NoticeV2SolrField;
import fr.abes.periscope.core.entity.solr.v2.ResultSolr;
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
@SpringBootTest(classes = {ModelMapper.class, DtoMapper.class})
public class DtoMapperTest {
    @Autowired
    private DtoMapper mapper;

    private NoticeV2 notice1;
    private NoticeV2 notice2;

    @BeforeEach
    void init() {
        notice1 = new NoticeV2();
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

        notice2 = new NoticeV2();
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
        Assertions.assertEquals("test title from different author", noticeWebV2Dto.getTitreAuteurDifferent());
        Assertions.assertEquals("test keyTitle", noticeWebV2Dto.getTitreCle());
        Assertions.assertEquals("test keyShortedTitle", noticeWebV2Dto.getTitreCleCourt());
        Assertions.assertEquals("test keyTitleQualifier", noticeWebV2Dto.getTitreCleQualifie());
        Assertions.assertEquals("test title complement", noticeWebV2Dto.getTitreComplement());
        Assertions.assertEquals("test parallel title", noticeWebV2Dto.getTitreParallele());
        Assertions.assertEquals("test proper title", noticeWebV2Dto.getTitrePropre());
        Assertions.assertEquals("test section title", noticeWebV2Dto.getTitreSection());
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

        FacetteSolr facette1 = new FacetteSolr(NoticeV2SolrField.DOCUMENT_TYPE);
        Map<String, Integer> mapFacette = new HashMap<>();
        mapFacette.put("Collection", 10);
        facette1.addValeurs(mapFacette);
        FacetteSolr facette2 = new FacetteSolr(NoticeV2SolrField.LANGUAGE);
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

}
