package fr.abes.periscope.core.repository.solr;

import fr.abes.periscope.core.configuration.SolRConfig;
import fr.abes.periscope.core.criterion.Criterion;
import fr.abes.periscope.core.criterion.CriterionPcp;
import fr.abes.periscope.core.criterion.CriterionRcr;
import fr.abes.periscope.core.entity.NoticeSolr;
import fr.abes.periscope.core.repository.NoticeRepository;
import fr.abes.periscope.core.repository.solr.NoticeField;
import fr.abes.periscope.core.repository.solr.SolrQueryBuilder;
import fr.abes.periscope.core.repository.solr.impl.AdvancedNoticeRepositoryImpl;
import org.junit.Assert;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledIf;
import org.junit.jupiter.api.condition.DisabledIfEnvironmentVariable;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.autoconfigure.domain.EntityScan;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.annotation.Profile;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.data.solr.core.DefaultQueryParser;
import org.springframework.data.solr.core.query.SimpleQuery;
import org.springframework.test.annotation.IfProfileValue;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.context.junit4.SpringRunner;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

@SpringBootTest(classes = {AdvancedNoticeRepositoryImpl.class, SolrQueryBuilder.class, SolRConfig.class,NoticeSolr.class})
@IfProfileValue(name ="spring.profiles.active", values ={"test-solr"})
public class SolrIntegrationTest {

    @Autowired
    private AdvancedNoticeRepositoryImpl noticeRepository;

    /**
     * Test de l'historiette #id 12
     */
    @Test
    public void testId12() {

        // Requête de Periscope V1
        String originalQuery = "930-z_t:PCCor";

        List<NoticeSolr> originalCandidates = noticeRepository.findNoticesBySolrQuery(originalQuery, PageRequest.of(0,10000,
                Sort.by(Sort.Direction.ASC, NoticeField.PPN)));

        // Requête par Periscope V2
        List<Criterion> criteria = new LinkedList<>();
        List<String> pcp = Arrays.asList("PCCor");
        CriterionPcp criterionPcp = new CriterionPcp(pcp);
        criteria.add(criterionPcp);

       List<NoticeSolr> newCandidates = noticeRepository.findNoticesByCriteria(criteria, PageRequest.of(0,10000,
               Sort.by(Sort.Direction.ASC, NoticeField.PPN)));

        Assert.assertEquals(originalCandidates,newCandidates);

    }

    /**
     * Test de l'historiette #id201
     */
    @Test
    public void testId201() {

        // Requête de Periscope V1
        String originalQuery = "(930-z_s:PCCor OR 930-z_s:PCPACA)";
        List<NoticeSolr> originalCandidates = noticeRepository.findNoticesBySolrQuery(originalQuery, PageRequest.of(0,25,
                Sort.by(Sort.Direction.ASC, NoticeField.PPN)));

        // Requête par Periscope V2
        List<Criterion> criteria = new LinkedList<>();
        List<String> pcp = Arrays.asList("PCCor","PCPACA");
        CriterionPcp criterionPcp = new CriterionPcp(pcp);
        criteria.add(criterionPcp);

        List<NoticeSolr> newCandidates = noticeRepository.findNoticesByCriteria(criteria, PageRequest.of(0,25,
                Sort.by(Sort.Direction.ASC, NoticeField.PPN)));

        Assert.assertEquals(originalCandidates,newCandidates);
    }

    /**
     * Test de l'historiette #id210
     */
    @Test
    public void testId210() {

        // Requête de Periscope V1
        String originalQuery = "(930-b_t:200336201  930-b_t:200962101)";
        List<NoticeSolr> originalCandidates = noticeRepository.findNoticesBySolrQuery(originalQuery, PageRequest.of(0,25,
                Sort.by(Sort.Direction.ASC, NoticeField.PPN)));

        // Requête par Periscope V2
        List<Criterion> criteria = new LinkedList<>();
        List<String> rcr = Arrays.asList("200336201","200962101");
        List<String> operator = Arrays.asList("ET","ET");
        CriterionRcr criterionRcr = new CriterionRcr(rcr,operator);
        criteria.add(criterionRcr);

        List<NoticeSolr> newCandidates = noticeRepository.findNoticesByCriteria(criteria, PageRequest.of(0,25,
                Sort.by(Sort.Direction.ASC, NoticeField.PPN)));

        Assert.assertEquals(originalCandidates,newCandidates);
    }

    /**
     * Test de l'historiette #id211
     */
    @Test
    public void testId211() {

        // Requête de Periscope V1
        String originalQuery = "(930-b_t:200336201 OR 930-b_t:200962101) ";
        List<NoticeSolr> originalCandidates = noticeRepository.findNoticesBySolrQuery(originalQuery, PageRequest.of(0,25,
                Sort.by(Sort.Direction.ASC, NoticeField.PPN)));

        // Requête par Periscope V2
        List<Criterion> criteria = new LinkedList<>();
        List<String> rcr = Arrays.asList("200336201","200962101");
        List<String> operator = Arrays.asList("ET","OU");
        CriterionRcr criterionRcr = new CriterionRcr(rcr,operator);
        criteria.add(criterionRcr);

        List<NoticeSolr> newCandidates = noticeRepository.findNoticesByCriteria(criteria, PageRequest.of(0,25,
                Sort.by(Sort.Direction.ASC, NoticeField.PPN)));

        Assert.assertEquals(originalCandidates,newCandidates);
    }

    /**
     * Test de l'historiette #id212
     */
    @Test
    public void testId212() {

        // Requête de Periscope V1
        String originalQuery = "(930-b_t:200336201 AND 930-b_t:200962101)";
        List<NoticeSolr> originalCandidates = noticeRepository.findNoticesBySolrQuery(originalQuery, PageRequest.of(0,25,
                Sort.by(Sort.Direction.ASC, NoticeField.PPN)));

        // Requête par Periscope V2
        List<Criterion> criteria = new LinkedList<>();
        List<String> rcr = Arrays.asList("200336201","200962101");
        List<String> operator = Arrays.asList("ET","ET");
        CriterionRcr criterionRcr = new CriterionRcr(rcr,operator);
        criteria.add(criterionRcr);

        List<NoticeSolr> newCandidates = noticeRepository.findNoticesByCriteria(criteria, PageRequest.of(0,25,
                Sort.by(Sort.Direction.ASC, NoticeField.PPN)));

        Assert.assertEquals(originalCandidates,newCandidates);
    }

    /**
     * Test de l'historiette #idX1
     */
    @Test
    public void testIdX1() {

        // Requête de Periscope V1
        String originalQuery = "((930-z_s:PCAq OR 930-z_s:PCAuv OR 930-z_s:PCBo OR 930-z_s:PCBre OR 930-z_s:PCCA OR 930-z_s:PCCAPI OR 930-z_s:PCCor OR 930-z_s:PCFC OR 930-z_s:PCLR OR 930-z_s:PCLim OR 930-z_s:PCLor OR 930-z_s:PCMP OR 930-z_s:PCNPDC OR 930-z_s:PCPACA OR 930-z_s:PCPCh OR 930-z_s:PCPL OR 930-z_s:PCPic OR 930-z_s:PCRA OR 930-z_s:PCSAM OR 930-z_s:PCSCen OR 930-z_s:PCUP OR 930-z_s:PCUR OR 930-z_s:PCAM OR 930-z_s:PCAS OR 930-z_s:PCAnt OR 930-z_s:PCChimie OR 930-z_s:PCDroit OR 930-z_s:PCEBCO OR 930-z_s:PCGer OR 930-z_s:PCG%C3%A9o OR 930-z_s:PCIta OR 930-z_s:PCMath OR 930-z_s:PCMed OR 930-z_s:PCMedieval OR 930-z_s:PCNum OR 930-z_s:PCPhilo OR 930-z_s:PCPhy OR 930-z_s:PCPsy OR 930-z_s:PCSTAPS) AND NOT 930-b_t:751052105)";
        List<NoticeSolr> originalCandidates = noticeRepository.findNoticesBySolrQuery(originalQuery, PageRequest.of(0,25,
                Sort.by(Sort.Direction.ASC, NoticeField.PPN)));

        // Requête par Periscope V2
        List<Criterion> criteria = new LinkedList<>();

        List<String> pcp = Arrays.asList("PCAq","PCAuv","PCBo","PCBre","PCCA","PCCAPI","PCCor","PCFC","PCLR","PCLim","PCLor","PCMP","PCNPDC","PCPACA","PCPCh","PCPL","PCPic","PCRA","PCSAM","PCSCen","930-z_s:PCUP","PCUR","PCAM","PCAS","PCAnt","PCChimie","PCDroit","PCEBCO","PCGer","PCGéo","PCIta","PCMath","PCMed","PCMedieval","PCNum","PCPhilo","PCPhy", "PCPsy", "PCSTAPS");
        CriterionPcp criterionPcp = new CriterionPcp(pcp);
        criteria.add(criterionPcp);

        List<String> rcr = Arrays.asList("751052105");
        List<String> operator = Arrays.asList("ET");
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
    public void testIdX2() {

        // Requête de Periscope V1
        String originalQuery = "( pcprcr_t:674821001PCAq OR pcprcr_t:674821001PCAuv OR pcprcr_t:674821001PCBo OR pcprcr_t:674821001PCBre OR pcprcr_t:674821001PCCA OR pcprcr_t:674821001PCCAPI OR pcprcr_t:674821001PCCor OR pcprcr_t:674821001PCFC OR pcprcr_t:674821001PCLR OR pcprcr_t:674821001PCLim OR pcprcr_t:674821001PCLor OR pcprcr_t:674821001PCMP OR pcprcr_t:674821001PCNPDC OR pcprcr_t:674821001PCPACA OR pcprcr_t:674821001PCPCh OR pcprcr_t:674821001PCPL OR pcprcr_t:674821001PCPic OR pcprcr_t:674821001PCRA OR pcprcr_t:674821001PCSAM OR pcprcr_t:674821001PCSCen OR pcprcr_t:674821001PCUP OR pcprcr_t:674821001PCUR OR pcprcr_t:674821001PCAM OR pcprcr_t:674821001PCAS OR pcprcr_t:674821001PCAnt OR pcprcr_t:674821001PCChimie OR pcprcr_t:674821001PCDroit OR pcprcr_t:674821001PCEBCO OR pcprcr_t:674821001PCGer OR pcprcr_t:674821001PCG%C3%A9o OR pcprcr_t:674821001PCIta OR pcprcr_t:674821001PCMath OR pcprcr_t:674821001PCMed OR pcprcr_t:674821001PCMedieval OR pcprcr_t:674821001PCNum OR pcprcr_t:674821001PCPhilo OR pcprcr_t:674821001PCPhy OR pcprcr_t:674821001PCPsy OR pcprcr_t:674821001PCSTAPS)";
        List<NoticeSolr> originalCandidates = noticeRepository.findNoticesBySolrQuery(originalQuery, PageRequest.of(0,25,
                Sort.by(Sort.Direction.ASC, NoticeField.PPN)));

        // Requête par Periscope V2
        List<Criterion> criteria = new LinkedList<>();

        List<String> pcp = Arrays.asList("PCAq","PCAuv","PCBo","PCBre","PCCA","PCCAPI","PCCor","PCFC","PCLR","PCLim","PCLor","PCMP","PCNPDC","PCPACA","PCPCh","PCPL","PCPic","PCRA","PCSAM","PCSCen","930-z_s:PCUP","PCUR","PCAM","PCAS","PCAnt","PCChimie","PCDroit","PCEBCO","PCGer","PCGéo","PCIta","PCMath","PCMed","PCMedieval","PCNum","PCPhilo","PCPhy", "PCPsy", "PCSTAPS");
        CriterionPcp criterionPcp = new CriterionPcp(pcp);
        criteria.add(criterionPcp);

        List<String> rcr = Arrays.asList("674821001");
        List<String> operator = Arrays.asList("ET");
        CriterionRcr criterionRcr = new CriterionRcr("ET",rcr,operator);
        criteria.add(criterionRcr);

        List<NoticeSolr> newCandidates = noticeRepository.findNoticesByCriteria(criteria, PageRequest.of(0,25,
                Sort.by(Sort.Direction.ASC, NoticeField.PPN)));

        Assert.assertEquals(originalCandidates,newCandidates);
    }

    /**
     * Test de l'historiette #idX3
     */
    @Test
    public void testIdX3() {

        // Requête de Periscope V1
        String originalQuery = "(930-z_s:PCDroit OR (930-b_t:212312101 OR 930-b_t:341722102))";
        List<NoticeSolr> originalCandidates = noticeRepository.findNoticesBySolrQuery(originalQuery, PageRequest.of(0,25,
                Sort.by(Sort.Direction.ASC, NoticeField.PPN)));

        // Requête par Periscope V2
        List<Criterion> criteria = new LinkedList<>();

        List<String> pcp = Arrays.asList("PCDroit");
        CriterionPcp criterionPcp = new CriterionPcp(pcp);
        criteria.add(criterionPcp);

        List<String> rcr = Arrays.asList("212312101","341722102");
        List<String> operator = Arrays.asList("ET","OU");
        CriterionRcr criterionRcr = new CriterionRcr("OU",rcr,operator);
        criteria.add(criterionRcr);

        List<NoticeSolr> newCandidates = noticeRepository.findNoticesByCriteria(criteria, PageRequest.of(0,25,
                Sort.by(Sort.Direction.ASC, NoticeField.PPN)));

        Assert.assertEquals(originalCandidates,newCandidates);
    }

    /**
     * Test de l'historiette #idX4
     */
    @Test
    public void testIdX4() {

        String originalQuery = "((930-z_s:PCDroit OR 930-z_s:PCPhilo) OR (930-b_t:212312101 OR 930-b_t:341722102))";
        List<NoticeSolr> originalCandidates = noticeRepository.findNoticesBySolrQuery(originalQuery, PageRequest.of(0,25,
                Sort.by(Sort.Direction.ASC, NoticeField.PPN)));

        List<Criterion> criteria = new LinkedList<>();

        List<String> pcp = Arrays.asList("PCDroit","PCPhilo");
        CriterionPcp criterionPcp = new CriterionPcp(pcp);
        criteria.add(criterionPcp);

        List<String> rcr = Arrays.asList("212312101","341722102");
        List<String> operator = Arrays.asList("ET","OU");
        CriterionRcr criterionRcr = new CriterionRcr("OU",rcr,operator);
        criteria.add(criterionRcr);

        List<NoticeSolr> newCandidates = noticeRepository.findNoticesByCriteria(criteria, PageRequest.of(0,25,
                Sort.by(Sort.Direction.ASC, NoticeField.PPN)));

        Assert.assertEquals(originalCandidates,newCandidates);
    }

}
