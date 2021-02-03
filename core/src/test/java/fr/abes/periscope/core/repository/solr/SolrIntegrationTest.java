package fr.abes.periscope.core.repository.solr;

import fr.abes.periscope.core.EnableOnIntegrationTest;
import fr.abes.periscope.core.configuration.SolRConfig;
import fr.abes.periscope.core.criterion.Criterion;
import fr.abes.periscope.core.criterion.CriterionPcp;
import fr.abes.periscope.core.criterion.CriterionPpn;
import fr.abes.periscope.core.criterion.CriterionRcr;
import fr.abes.periscope.core.criterion.CriterionTitleWords;
import fr.abes.periscope.core.entity.NoticeSolr;
import fr.abes.periscope.core.repository.solr.impl.AdvancedNoticeRepositoryImpl;
import org.junit.Assert;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.data.solr.core.DefaultQueryParser;
import org.springframework.data.solr.core.query.SimpleQuery;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

@EnableOnIntegrationTest
@SpringBootTest(classes = {AdvancedNoticeRepositoryImpl.class, SolrQueryBuilder.class, SolRConfig.class,NoticeSolr.class})
public class SolrIntegrationTest {

    @Autowired
    private AdvancedNoticeRepositoryImpl noticeRepository;

    /**
     * Test de l'historiette #id 12
     */
    @Test
    @DisplayName("historiette #id 12")
    public void testId12() {

        // Requête de Periscope V1
        String originalQuery = "930-z_t:PCCor";

        List<NoticeSolr> originalCandidates = noticeRepository.findNoticesBySolrQuery(originalQuery, PageRequest.of(0,25,
                Sort.by(Sort.Direction.ASC, NoticeField.PPN)));

        // Requête par Periscope V2
        List<Criterion> criteria = new LinkedList<>();
        List<String> pcp = Arrays.asList("PCCor");
        List<String> pcpOperator = Arrays.asList("OU");
        CriterionPcp criterionPcp = new CriterionPcp(pcp,pcpOperator);
        criteria.add(criterionPcp);

       List<NoticeSolr> newCandidates = noticeRepository.findNoticesByCriteria(criteria, PageRequest.of(0,25,
               Sort.by(Sort.Direction.ASC, NoticeField.PPN)));

        Assert.assertEquals(originalCandidates,newCandidates);

    }

    /**
     * Test de l'historiette #id201
     */
    @Test
    @DisplayName("historiette #id 201")
    public void testId201() {

        // Requête de Periscope V1
        String originalQuery = "(930-z_s:PCCor OR 930-z_s:PCPACA)";
        List<NoticeSolr> originalCandidates = noticeRepository.findNoticesBySolrQuery(originalQuery, PageRequest.of(0,25,
                Sort.by(Sort.Direction.ASC, NoticeField.PPN)));

        // Requête par Periscope V2
        List<Criterion> criteria = new LinkedList<>();
        List<String> pcp = Arrays.asList("PCCor","PCPACA");
        List<String> pcpOperator = Arrays.asList("OU","OU");
        CriterionPcp criterionPcp = new CriterionPcp(pcp,pcpOperator);
        criteria.add(criterionPcp);

        List<NoticeSolr> newCandidates = noticeRepository.findNoticesByCriteria(criteria, PageRequest.of(0,25,
                Sort.by(Sort.Direction.ASC, NoticeField.PPN)));

        Assert.assertEquals(originalCandidates,newCandidates);
    }

    /**
     * Test de l'historiette #id202
     */
    @Test
    @DisplayName("historiette #id 202")
    public void testId202() {
        // Requête de Periscope V1
        String originalQuery = "001_s:038640139 AND (930-z_s:PCCor)";
        List<NoticeSolr> originalCandidates = noticeRepository.findNoticesBySolrQuery(originalQuery, PageRequest.of(0,25,
                Sort.by(Sort.Direction.ASC, NoticeField.PPN)));

        // Requête par Periscope V2
        List<Criterion> criteria = new LinkedList<>();

        List<String> ppn = Arrays.asList("038640139");
        CriterionPpn criterionPpn = new CriterionPpn(ppn);
        criteria.add(criterionPpn);

        List<String> pcp = Arrays.asList("PCCor");
        List<String> pcpOperator = Arrays.asList("OU");
        CriterionPcp criterionPcp = new CriterionPcp("ET",pcp,pcpOperator);
        criteria.add(criterionPcp);

        List<NoticeSolr> newCandidates = noticeRepository.findNoticesByCriteria(criteria, PageRequest.of(0,25,
                Sort.by(Sort.Direction.ASC, NoticeField.PPN)));

        Assert.assertEquals(originalCandidates,newCandidates);
    }

    /**
     * Test de l'historiette #id203
     */
    @Test
    @DisplayName("historiette #id 203")
    public void testId203() {
        // Requête de Periscope V1
        String originalQuery = "930-z_s:PCCor AND -(001_s:038640140)";
        List<NoticeSolr> originalCandidates = noticeRepository.findNoticesBySolrQuery(originalQuery, PageRequest.of(0,25,
                Sort.by(Sort.Direction.ASC, NoticeField.PPN)));

        // Requête par Periscope V2
        List<Criterion> criteria = new LinkedList<>();

        List<String> pcp = Arrays.asList("PCCor");
        List<String> pcpOperator = Arrays.asList("OU");
        CriterionPcp criterionPcp = new CriterionPcp(pcp,pcpOperator);

        criteria.add(criterionPcp);

        List<String> ppn = Arrays.asList("038640140");
        CriterionPpn criterionPpn = new CriterionPpn("SAUF", ppn);

        criteria.add(criterionPpn);

        List<NoticeSolr> newCandidates = noticeRepository.findNoticesByCriteria(criteria, PageRequest.of(0,25,
                Sort.by(Sort.Direction.ASC, NoticeField.PPN)));

        Assert.assertEquals(originalCandidates,newCandidates);
    }

    /**
     * Test de l'historiette #id204
     */
    @Test
    @DisplayName("historiette #id 204")
    public void testId204() {
        // Requête de Periscope V1
        String originalQuery = "930-z_s:PCCor OR 001_s:039612473";
        List<NoticeSolr> originalCandidates = noticeRepository.findNoticesBySolrQuery(originalQuery, PageRequest.of(0,25,
                Sort.by(Sort.Direction.ASC, NoticeField.PPN)));

        List<Criterion> criteria = new LinkedList<>();

        List<String> pcp = Arrays.asList("PCCor");
        List<String> pcpOperator = Arrays.asList("OU");
        CriterionPcp criterionPcp = new CriterionPcp(pcp,pcpOperator);

        criteria.add(criterionPcp);

        List<String> ppn = Arrays.asList("039612473");
        CriterionPpn criterionPpn = new CriterionPpn("OU", ppn);

        criteria.add(criterionPpn);

        List<NoticeSolr> newCandidates = noticeRepository.findNoticesByCriteria(criteria, PageRequest.of(0,25,
                Sort.by(Sort.Direction.ASC, NoticeField.PPN)));

        Assert.assertEquals(originalCandidates,newCandidates);
    }

    /**
     * Test de l'historiette #id210
     */
    @Test
    @DisplayName("historiette #id 210")
    public void testId210() {

        // Requête de Periscope V1
        String originalQuery = "(930-b_t:200336201  930-b_t:200962101)";
        List<NoticeSolr> originalCandidates = noticeRepository.findNoticesBySolrQuery(originalQuery, PageRequest.of(0,25,
                Sort.by(Sort.Direction.ASC, NoticeField.PPN)));

        // Requête par Periscope V2
        List<Criterion> criteria = new LinkedList<>();
        List<String> rcr = Arrays.asList("200336201","200962101");
        List<String> rcrOperator = Arrays.asList("ET","OU");
        CriterionRcr criterionRcr = new CriterionRcr(rcr,rcrOperator);
        criteria.add(criterionRcr);

        List<NoticeSolr> newCandidates = noticeRepository.findNoticesByCriteria(criteria, PageRequest.of(0,25,
                Sort.by(Sort.Direction.ASC, NoticeField.PPN)));

        Assert.assertEquals(originalCandidates,newCandidates);
    }

    /**
     * Test de l'historiette #id211
     */
    @Test
    @DisplayName("historiette #id 211")
    public void testId211() {

        // Requête de Periscope V1
        String originalQuery = "(930-b_t:200336201 OR 930-b_t:200962101)";
        List<NoticeSolr> originalCandidates = noticeRepository.findNoticesBySolrQuery(originalQuery, PageRequest.of(0,25,
                Sort.by(Sort.Direction.ASC, NoticeField.PPN)));

        // Requête par Periscope V2
        List<Criterion> criteria = new LinkedList<>();
        List<String> rcr = Arrays.asList("200336201","200962101");
        List<String> rcrOperator = Arrays.asList("ET","OU");
        CriterionRcr criterionRcr = new CriterionRcr(rcr,rcrOperator);
        criteria.add(criterionRcr);

        List<NoticeSolr> newCandidates = noticeRepository.findNoticesByCriteria(criteria, PageRequest.of(0,25,
                Sort.by(Sort.Direction.ASC, NoticeField.PPN)));

        Assert.assertEquals(originalCandidates,newCandidates);
    }

    /**
     * Test de l'historiette #id212
     */
    @Test
    @DisplayName("historiette #id 212")
    public void testId212() {

        // Requête de Periscope V1
        String originalQuery = "(930-b_t:200336201 AND 930-b_t:200962101)";
        List<NoticeSolr> originalCandidates = noticeRepository.findNoticesBySolrQuery(originalQuery, PageRequest.of(0,25,
                Sort.by(Sort.Direction.ASC, NoticeField.PPN)));

        // Requête par Periscope V2
        List<Criterion> criteria = new LinkedList<>();
        List<String> rcr = Arrays.asList("200336201","200962101");
        List<String> rcrOperator = Arrays.asList("ET","ET");
        CriterionRcr criterionRcr = new CriterionRcr(rcr,rcrOperator);
        criteria.add(criterionRcr);

        List<NoticeSolr> newCandidates = noticeRepository.findNoticesByCriteria(criteria, PageRequest.of(0,25,
                Sort.by(Sort.Direction.ASC, NoticeField.PPN)));

        Assert.assertEquals(originalCandidates,newCandidates);
    }

    /**
     * Test de l'historiette #id214
     */
    @Test
    @DisplayName("historiette #id 214")
    public void testId214() {

        // Requête de Periscope V1
        String originalQuery = "930-z_s:PCCor OR ((530-a_t:\"corse\"~5 OR 531-a_t:\"corse\"~5 OR 200-a_t:\"corse\"~5 OR 200-c_t:\"corse\"~5 OR 200-d_t:\"corse\"~5 OR 200-e_t:\"corse\"~5 OR 200-i_t:\"corse\"~5))";
        List<NoticeSolr> originalCandidates = noticeRepository.findNoticesBySolrQuery(originalQuery, PageRequest.of(0,25,
                Sort.by(Sort.Direction.ASC, NoticeField.PPN)));

        // Requête par Periscope V2
        List<Criterion> criteria = new LinkedList<>();
        List<String> pcp = Arrays.asList("PCCor");
        List<String> pcpOperator = Arrays.asList("OU");
        CriterionPcp criterionPcp = new CriterionPcp(pcp,pcpOperator);
        criteria.add(criterionPcp);

        List<String> titleWords = Arrays.asList("corse");
        List<String> titleWordsOperator = Arrays.asList("ET");
        CriterionTitleWords criterionTitleWords = new CriterionTitleWords("OU",titleWords,titleWordsOperator);
        criteria.add(criterionTitleWords);

        List<NoticeSolr> newCandidates = noticeRepository.findNoticesByCriteria(criteria, PageRequest.of(0,25,
                Sort.by(Sort.Direction.ASC, NoticeField.PPN)));

        Assert.assertEquals(originalCandidates,newCandidates);
    }

    /**
     * Test de l'historiette #id215
     */
    @Test
    @DisplayName("historiette #id 215")
    public void testId215() {

        // Requête de Periscope V1
        String originalQuery = "930-z_s:PCCor AND ((530-a_t:\"corse\"~5 OR 531-a_t:\"corse\"~5 OR 200-a_t:\"corse\"~5 OR 200-c_t:\"corse\"~5 OR 200-d_t:\"corse\"~5 OR 200-e_t:\"corse\"~5 OR 200-i_t:\"corse\"~5))";
        List<NoticeSolr> originalCandidates = noticeRepository.findNoticesBySolrQuery(originalQuery, PageRequest.of(0,25,
                Sort.by(Sort.Direction.ASC, NoticeField.PPN)));

        // Requête par Periscope V2
        List<Criterion> criteria = new LinkedList<>();
        List<String> pcp = Arrays.asList("PCCor");
        List<String> pcpOperator = Arrays.asList("OU");
        CriterionPcp criterionPcp = new CriterionPcp(pcp,pcpOperator);
        criteria.add(criterionPcp);

        List<String> titleWords = Arrays.asList("corse");
        List<String> titleWordsOperator = Arrays.asList("ET");
        CriterionTitleWords criterionTitleWords = new CriterionTitleWords("ET",titleWords,titleWordsOperator);
        criteria.add(criterionTitleWords);

        List<NoticeSolr> newCandidates = noticeRepository.findNoticesByCriteria(criteria, PageRequest.of(0,25,
                Sort.by(Sort.Direction.ASC, NoticeField.PPN)));

        Assert.assertEquals(originalCandidates,newCandidates);
    }

    /**
     * Test de l'historiette #id216
     */
    @Test
    @DisplayName("historiette #id 216")
    public void testId216() {

        // Requête de Periscope V1
        String originalQuery = "930-z_s:PCCor AND NOT ((530-a_t:\"corse\"~5 OR 531-a_t:\"corse\"~5 OR 200-a_t:\"corse\"~5 OR 200-c_t:\"corse\"~5 OR 200-d_t:\"corse\"~5 OR 200-e_t:\"corse\"~5 OR 200-i_t:\"corse\"~5))";
        List<NoticeSolr> originalCandidates = noticeRepository.findNoticesBySolrQuery(originalQuery, PageRequest.of(0,25,
                Sort.by(Sort.Direction.ASC, NoticeField.PPN)));

        // Requête par Periscope V2
        List<Criterion> criteria = new LinkedList<>();
        List<String> pcp = Arrays.asList("PCCor");
        List<String> pcpOperator = Arrays.asList("OU");
        CriterionPcp criterionPcp = new CriterionPcp(pcp,pcpOperator);
        criteria.add(criterionPcp);

        List<String> titleWords = Arrays.asList("corse");
        List<String> tileWordsOperator = Arrays.asList("ET");
        CriterionTitleWords criterionTitleWords = new CriterionTitleWords("SAUF",titleWords,tileWordsOperator);
        criteria.add(criterionTitleWords);

        List<NoticeSolr> newCandidates = noticeRepository.findNoticesByCriteria(criteria, PageRequest.of(0,25,
                Sort.by(Sort.Direction.ASC, NoticeField.PPN)));

        Assert.assertEquals(originalCandidates,newCandidates);
    }

    /**
     * Test de l'historiette #idX1
     */
    @Test
    @DisplayName("historiette #id X1")
    public void testIdX1() {

        // Requête de Periscope V1
        String originalQuery = "((930-z_s:PCAq OR 930-z_s:PCAuv OR 930-z_s:PCBo OR 930-z_s:PCBre OR 930-z_s:PCCA OR 930-z_s:PCCAPI OR 930-z_s:PCCor OR 930-z_s:PCFC OR 930-z_s:PCLR OR 930-z_s:PCLim OR 930-z_s:PCLor OR 930-z_s:PCMP OR 930-z_s:PCNPDC OR 930-z_s:PCPACA OR 930-z_s:PCPCh OR 930-z_s:PCPL OR 930-z_s:PCPic OR 930-z_s:PCRA OR 930-z_s:PCSAM OR 930-z_s:PCSCen OR 930-z_s:PCUP OR 930-z_s:PCUR OR 930-z_s:PCAM OR 930-z_s:PCAS OR 930-z_s:PCAnt OR 930-z_s:PCChimie OR 930-z_s:PCDroit OR 930-z_s:PCEBCO OR 930-z_s:PCGer OR 930-z_s:PCGéo OR 930-z_s:PCIta OR 930-z_s:PCMath OR 930-z_s:PCMed OR 930-z_s:PCMedieval OR 930-z_s:PCNum OR 930-z_s:PCPhilo OR 930-z_s:PCPhy OR 930-z_s:PCPsy OR 930-z_s:PCSTAPS) AND NOT 930-b_t:751052105)";
        List<NoticeSolr> originalCandidates = noticeRepository.findNoticesBySolrQuery(originalQuery, PageRequest.of(0,25,
                Sort.by(Sort.Direction.ASC, NoticeField.PPN)));

        // Requête par Periscope V2
        List<Criterion> criteria = new LinkedList<>();

        List<String> pcp = Arrays.asList("PCAq","PCAuv","PCBo","PCBre","PCCA","PCCAPI","PCCor","PCFC","PCLR","PCLim","PCLor","PCMP","PCNPDC","PCPACA","PCPCh","PCPL","PCPic","PCRA","PCSAM","PCSCen","PCUP","PCUR","PCAM","PCAS","PCAnt","PCChimie","PCDroit","PCEBCO","PCGer","PCGéo","PCIta","PCMath","PCMed","PCMedieval","PCNum","PCPhilo","PCPhy", "PCPsy", "PCSTAPS");
        List<String> pcpOperator = Arrays.asList("OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU");
        CriterionPcp criterionPcp = new CriterionPcp(pcp,pcpOperator);
        criteria.add(criterionPcp);

        List<String> rcr = Arrays.asList("751052105");
        List<String> operator = Arrays.asList("OU");
        CriterionRcr criterionRcr = new CriterionRcr("SAUF",rcr,operator);
        criteria.add(criterionRcr);

        List<NoticeSolr> newCandidates = noticeRepository.findNoticesByCriteria(criteria, PageRequest.of(0,25,
                Sort.by(Sort.Direction.ASC, NoticeField.PPN)));

        Assert.assertEquals(originalCandidates,newCandidates);
    }

    /**
     * Test de l'historiette #idX2
     */
    @Test
    @DisplayName("historiette #id X2")
    public void testIdX2() {

        // Requête de Periscope V1
        //String originalQuery = "( pcprcr_t:674821001PCAq OR pcprcr_t:674821001PCAuv OR pcprcr_t:674821001PCBo OR pcprcr_t:674821001PCBre OR pcprcr_t:674821001PCCA OR pcprcr_t:674821001PCCAPI OR pcprcr_t:674821001PCCor OR pcprcr_t:674821001PCFC OR pcprcr_t:674821001PCLR OR pcprcr_t:674821001PCLim OR pcprcr_t:674821001PCLor OR pcprcr_t:674821001PCMP OR pcprcr_t:674821001PCNPDC OR pcprcr_t:674821001PCPACA OR pcprcr_t:674821001PCPCh OR pcprcr_t:674821001PCPL OR pcprcr_t:674821001PCPic OR pcprcr_t:674821001PCRA OR pcprcr_t:674821001PCSAM OR pcprcr_t:674821001PCSCen OR pcprcr_t:674821001PCUP OR pcprcr_t:674821001PCUR OR pcprcr_t:674821001PCAM OR pcprcr_t:674821001PCAS OR pcprcr_t:674821001PCAnt OR pcprcr_t:674821001PCChimie OR pcprcr_t:674821001PCDroit OR pcprcr_t:674821001PCEBCO OR pcprcr_t:674821001PCGer OR pcprcr_t:674821001PCGéo OR pcprcr_t:674821001PCIta OR pcprcr_t:674821001PCMath OR pcprcr_t:674821001PCMed OR pcprcr_t:674821001PCMedieval OR pcprcr_t:674821001PCNum OR pcprcr_t:674821001PCPhilo OR pcprcr_t:674821001PCPhy OR pcprcr_t:674821001PCPsy OR pcprcr_t:674821001PCSTAPS)";
        // Requête comme Solr total
        String originalQuery = "( 930-z_s:PCAq OR  930-z_s:PCAuv OR  930-z_s:PCBo OR  930-z_s:PCBre OR  930-z_s:PCCA OR  930-z_s:PCCAPI OR  930-z_s:PCCor OR  930-z_s:PCFC OR  930-z_s:PCLR OR  930-z_s:PCLim OR  930-z_s:PCLor OR  930-z_s:PCMP OR  930-z_s:PCNPDC OR  930-z_s:PCPACA OR  930-z_s:PCPCh OR  930-z_s:PCPL OR  930-z_s:PCPic OR  930-z_s:PCRA OR  930-z_s:PCSAM OR  930-z_s:PCSCen OR  930-z_s:PCUP OR  930-z_s:PCUR OR  930-z_s:PCAM OR  930-z_s:PCAS OR  930-z_s:PCAnt OR  930-z_s:PCChimie OR  930-z_s:PCDroit OR  930-z_s:PCEBCO OR  930-z_s:PCGer OR  930-z_s:PCGéo OR  930-z_s:PCIta OR  930-z_s:PCMath OR  930-z_s:PCMed OR  930-z_s:PCMedieval OR  930-z_s:PCNum OR  930-z_s:PCPhilo OR  930-z_s:PCPhy OR  930-z_s:PCPsy OR  930-z_s:PCSTAPS) AND 930-b_t:674821001";

        List<NoticeSolr> originalCandidates = noticeRepository.findNoticesBySolrQuery(originalQuery, PageRequest.of(0,25,
                Sort.by(Sort.Direction.ASC, NoticeField.PPN)));

        // Requête par Periscope V2
        List<Criterion> criteria = new LinkedList<>();

        List<String> pcp = Arrays.asList("PCAq","PCAuv","PCBo","PCBre","PCCA","PCCAPI","PCCor","PCFC","PCLR","PCLim","PCLor","PCMP","PCNPDC","PCPACA","PCPCh","PCPL","PCPic","PCRA","PCSAM","PCSCen","PCUP","PCUR","PCAM","PCAS","PCAnt","PCChimie","PCDroit","PCEBCO","PCGer","PCGéo","PCIta","PCMath","PCMed","PCMedieval","PCNum","PCPhilo","PCPhy", "PCPsy", "PCSTAPS");
        List<String> pcpOperator = Arrays.asList("OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU");
        CriterionPcp criterionPcp = new CriterionPcp(pcp,pcpOperator);
        criteria.add(criterionPcp);

        List<String> rcr = Arrays.asList("674821001");
        List<String> rcrOperator = Arrays.asList("ET");
        CriterionRcr criterionRcr = new CriterionRcr("ET",rcr,rcrOperator);
        criteria.add(criterionRcr);

        List<NoticeSolr> newCandidates = noticeRepository.findNoticesByCriteria(criteria, PageRequest.of(0,25,
                Sort.by(Sort.Direction.ASC, NoticeField.PPN)));

        Assert.assertEquals(originalCandidates,newCandidates);
    }

    /**
     * Test de l'historiette #idX3
     */
    @Test
    @DisplayName("historiette #id X3")
    public void testIdX3() {

        // Requête de Periscope V1
        String originalQuery = "(930-z_s:PCDroit OR (930-b_t:212312101 OR 930-b_t:341722102))";
        List<NoticeSolr> originalCandidates = noticeRepository.findNoticesBySolrQuery(originalQuery, PageRequest.of(0,25,
                Sort.by(Sort.Direction.ASC, NoticeField.PPN)));

        // Requête par Periscope V2
        List<Criterion> criteria = new LinkedList<>();

        List<String> pcp = Arrays.asList("PCDroit");
        List<String> pcpOperator = Arrays.asList("OU");
        CriterionPcp criterionPcp = new CriterionPcp(pcp,pcpOperator);
        criteria.add(criterionPcp);

        List<String> rcr = Arrays.asList("212312101","341722102");
        List<String> rcrOperator = Arrays.asList("ET","OU");
        CriterionRcr criterionRcr = new CriterionRcr("OU",rcr,rcrOperator);
        criteria.add(criterionRcr);

        List<NoticeSolr> newCandidates = noticeRepository.findNoticesByCriteria(criteria, PageRequest.of(0,25,
                Sort.by(Sort.Direction.ASC, NoticeField.PPN)));

        Assert.assertEquals(originalCandidates,newCandidates);
    }

    /**
     * Test de l'historiette #idX4
     */
    @Test
    @DisplayName("historiette #id X4")
    public void testIdX4() {

        String originalQuery = "((930-z_s:PCDroit OR 930-z_s:PCPhilo) OR (930-b_t:212312101 OR 930-b_t:341722102))";
        List<NoticeSolr> originalCandidates = noticeRepository.findNoticesBySolrQuery(originalQuery, PageRequest.of(0,25,
                Sort.by(Sort.Direction.ASC, NoticeField.PPN)));

        List<Criterion> criteria = new LinkedList<>();

        List<String> pcp = Arrays.asList("PCDroit","PCPhilo");
        List<String> pcpOperator = Arrays.asList("OU","OU");
        CriterionPcp criterionPcp = new CriterionPcp(pcp,pcpOperator);
        criteria.add(criterionPcp);

        List<String> rcr = Arrays.asList("212312101","341722102");
        List<String> rcrOperator = Arrays.asList("ET","OU");
        CriterionRcr criterionRcr = new CriterionRcr("OU",rcr,rcrOperator);
        criteria.add(criterionRcr);

        List<NoticeSolr> newCandidates = noticeRepository.findNoticesByCriteria(criteria, PageRequest.of(0,25,
                Sort.by(Sort.Direction.ASC, NoticeField.PPN)));

        Assert.assertEquals(originalCandidates,newCandidates);
    }

}
