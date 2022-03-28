package fr.abes.periscope.core.repository.solr;

import fr.abes.periscope.core.CoreTestConfiguration;
import fr.abes.periscope.core.EnableOnIntegrationTest;
import fr.abes.periscope.core.entity.solr.v1.NoticeV1Solr;
import fr.abes.periscope.core.entity.solr.v1.NoticeV1SolrField;
import fr.abes.periscope.core.criterion.*;
import fr.abes.periscope.core.repository.solr.v1.impl.AdvancedNoticeSolrV1RepositoryImpl;
import org.junit.Assert;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;

import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

@EnableOnIntegrationTest
@SpringBootTest(classes = {CoreTestConfiguration.class})
public class SolrIntegrationTest {

    @Autowired
    private AdvancedNoticeSolrV1RepositoryImpl noticeRepository;

    /**
     * Test de l'historiette #id 12
     */
    @Test
    @DisplayName("historiette #id 12")
    void testId12() {
        Sort sort = new Sort(Sort.Direction.ASC, NoticeV1SolrField.PPN);

        // Requête de Periscope V1
        String originalQuery = "930-z_s:PCCor";
        List<NoticeV1Solr> originalCandidates = noticeRepository.findNoticesBySolrQuery(originalQuery, sort, PageRequest.of(0,25));

        // Requête par Periscope V2
        List<Criterion> criteria = new LinkedList<>();
        List<String> pcp = Collections.singletonList("PCCor");
        List<String> pcpOperator = Collections.singletonList("OU");
        CriterionPcp criterionPcp = new CriterionPcp(pcp,pcpOperator);
        criteria.add(criterionPcp);

        List<NoticeV1Solr> newCandidates = noticeRepository.findNoticesByCriteria(criteria, sort, PageRequest.of(0,25));

        Assert.assertEquals(originalCandidates,newCandidates);

    }

    /**
     * Test de l'historiette #id201
     */
    @Test
    @DisplayName("historiette #id 201")
    void testId201() {
        Sort sort = new Sort(Sort.Direction.ASC, NoticeV1SolrField.PPN);

        // Requête de Periscope V1
        String originalQuery = "(930-z_s:PCCor OR 930-z_s:PCPACA)";
        List<NoticeV1Solr> originalCandidates = noticeRepository.findNoticesBySolrQuery(originalQuery, sort, PageRequest.of(0,25));

        // Requête par Periscope V2
        List<Criterion> criteria = new LinkedList<>();
        List<String> pcp = Arrays.asList("PCCor","PCPACA");
        List<String> pcpOperator = Arrays.asList("OU","OU");
        CriterionPcp criterionPcp = new CriterionPcp(pcp,pcpOperator);
        criteria.add(criterionPcp);
        List<NoticeV1Solr> newCandidates = noticeRepository.findNoticesByCriteria(criteria, sort, PageRequest.of(0,25));

        Assert.assertEquals(originalCandidates,newCandidates);
    }

    /**
     * Test de l'historiette #id202
     */
    @Test
    @DisplayName("historiette #id 202")
    void testId202() {
        Sort sort = new Sort(Sort.Direction.ASC, NoticeV1SolrField.PPN);

        // Requête de Periscope V1
        String originalQuery = "001_s:038640139 AND (930-z_s:PCCor)";
        List<NoticeV1Solr> originalCandidates = noticeRepository.findNoticesBySolrQuery(originalQuery, sort, PageRequest.of(0,25));

        // Requête par Periscope V2
        List<Criterion> criteria = new LinkedList<>();

        List<String> ppn = Collections.singletonList("038640139");
        CriterionPpn criterionPpn = new CriterionPpn(ppn);
        criteria.add(criterionPpn);

        List<String> pcp = Collections.singletonList("PCCor");
        List<String> pcpOperator = Collections.singletonList("OU");
        CriterionPcp criterionPcp = new CriterionPcp("ET",pcp,pcpOperator);
        criteria.add(criterionPcp);

        List<NoticeV1Solr> newCandidates = noticeRepository.findNoticesByCriteria(criteria, sort, PageRequest.of(0,25));

        Assert.assertEquals(originalCandidates,newCandidates);
    }

    /**
     * Test de l'historiette #id203
     */
    @Test
    @DisplayName("historiette #id 203")
    void testId203() {
        Sort sort = new Sort(Sort.Direction.ASC, NoticeV1SolrField.PPN);

        // Requête de Periscope V1
        String originalQuery = "930-z_s:PCCor AND -(001_s:038640140)";
        List<NoticeV1Solr> originalCandidates = noticeRepository.findNoticesBySolrQuery(originalQuery, sort, PageRequest.of(0,25));

        // Requête par Periscope V2
        List<Criterion> criteria = new LinkedList<>();

        List<String> pcp = Collections.singletonList("PCCor");
        List<String> pcpOperator = Collections.singletonList("OU");
        CriterionPcp criterionPcp = new CriterionPcp(pcp,pcpOperator);
        criteria.add(criterionPcp);

        List<String> ppn = Collections.singletonList("038640140");
        CriterionPpn criterionPpn = new CriterionPpn("SAUF", ppn);
        criteria.add(criterionPpn);

        List<NoticeV1Solr> newCandidates = noticeRepository.findNoticesByCriteria(criteria, sort, PageRequest.of(0,25));

        Assert.assertEquals(originalCandidates,newCandidates);
    }

    /**
     * Test de l'historiette #id204
     */
    @Test
    @DisplayName("historiette #id 204")
    void testId204() {
        Sort sort = new Sort(Sort.Direction.ASC, NoticeV1SolrField.PPN);

        // Requête de Periscope V1
        String originalQuery = "930-z_s:PCCor OR 001_s:039612473";
        List<NoticeV1Solr> originalCandidates = noticeRepository.findNoticesBySolrQuery(originalQuery, sort, PageRequest.of(0,25));

        List<Criterion> criteria = new LinkedList<>();

        List<String> pcp = Collections.singletonList("PCCor");
        List<String> pcpOperator = Collections.singletonList("OU");
        CriterionPcp criterionPcp = new CriterionPcp(pcp,pcpOperator);
        criteria.add(criterionPcp);

        List<String> ppn = Arrays.asList("039612473");
        CriterionPpn criterionPpn = new CriterionPpn("OU", ppn);
        criteria.add(criterionPpn);

        List<NoticeV1Solr> newCandidates = noticeRepository.findNoticesByCriteria(criteria, sort, PageRequest.of(0,25));

        Assert.assertEquals(originalCandidates,newCandidates);
    }

    /**
     * Test de l'historiette #id210
     */
    @Test
    @DisplayName("historiette #id 210")
    void testId210() {
        Sort sort = new Sort(Sort.Direction.ASC, NoticeV1SolrField.PPN);

        // Requête de Periscope V1
        String originalQuery = "(930-b_t:200336201  930-b_t:200962101)";
        List<NoticeV1Solr> originalCandidates = noticeRepository.findNoticesBySolrQuery(originalQuery, sort, PageRequest.of(0,25));

        // Requête par Periscope V2
        List<Criterion> criteria = new LinkedList<>();
        List<String> rcr = Arrays.asList("200336201","200962101");
        List<String> rcrOperator = Arrays.asList("ET","OU");
        CriterionRcr criterionRcr = new CriterionRcr(rcr,rcrOperator);
        criteria.add(criterionRcr);

        List<NoticeV1Solr> newCandidates = noticeRepository.findNoticesByCriteria(criteria, sort, PageRequest.of(0,25));

        Assert.assertEquals(originalCandidates,newCandidates);
    }

    /**
     * Test de l'historiette #id211
     */
    @Test
    @DisplayName("historiette #id 211")
    void testId211() {
        Sort sort = new Sort(Sort.Direction.ASC, NoticeV1SolrField.PPN);

        // Requête de Periscope V1
        String originalQuery = "(930-b_t:200336201 OR 930-b_t:200962101)";
        List<NoticeV1Solr> originalCandidates = noticeRepository.findNoticesBySolrQuery(originalQuery, sort, PageRequest.of(0,25));

        // Requête par Periscope V2
        List<Criterion> criteria = new LinkedList<>();
        List<String> rcr = Arrays.asList("200336201","200962101");
        List<String> rcrOperator = Arrays.asList("ET","OU");
        CriterionRcr criterionRcr = new CriterionRcr(rcr,rcrOperator);
        criteria.add(criterionRcr);

        List<NoticeV1Solr> newCandidates = noticeRepository.findNoticesByCriteria(criteria, sort, PageRequest.of(0,25));

        Assert.assertEquals(originalCandidates,newCandidates);
    }

    /**
     * Test de l'historiette #id212
     */
    @Test
    @DisplayName("historiette #id 212")
    void testId212() {
        Sort sort = new Sort(Sort.Direction.ASC, NoticeV1SolrField.PPN);

        // Requête de Periscope V1
        String originalQuery = "(930-b_t:200336201 AND 930-b_t:200962101)";
        List<NoticeV1Solr> originalCandidates = noticeRepository.findNoticesBySolrQuery(originalQuery, sort, PageRequest.of(0,25));

        // Requête par Periscope V2
        List<Criterion> criteria = new LinkedList<>();
        List<String> rcr = Arrays.asList("200336201","200962101");
        List<String> rcrOperator = Arrays.asList("ET","ET");
        CriterionRcr criterionRcr = new CriterionRcr(rcr,rcrOperator);
        criteria.add(criterionRcr);

        List<NoticeV1Solr> newCandidates = noticeRepository.findNoticesByCriteria(criteria, sort, PageRequest.of(0,25));

        Assert.assertEquals(originalCandidates,newCandidates);
    }

    /**
     * Test de l'historiette #id214
     */
    @Test
    @DisplayName("historiette #id 214")
    void testId214() {
        Sort sort = new Sort(Sort.Direction.ASC, NoticeV1SolrField.PPN);

        // Requête de Periscope V1
        String originalQuery = "930-z_s:PCCor OR ((530-a_t:\"corse\"~5 OR 531-a_t:\"corse\"~5 OR 200-a_t:\"corse\"~5 OR 200-c_t:\"corse\"~5 OR 200-d_t:\"corse\"~5 OR 200-e_t:\"corse\"~5 OR 200-i_t:\"corse\"~5))";
        List<NoticeV1Solr> originalCandidates = noticeRepository.findNoticesBySolrQuery(originalQuery, sort, PageRequest.of(0,25));

        // Requête par Periscope V2
        List<Criterion> criteria = new LinkedList<>();
        List<String> pcp = Collections.singletonList("PCCor");
        List<String> pcpOperator = Collections.singletonList("ET");
        CriterionPcp criterionPcp = new CriterionPcp(pcp,pcpOperator);
        criteria.add(criterionPcp);

        List<String> titleWords = Collections.singletonList("corse");
        List<String> titleWordsOperator = Collections.singletonList("ET");
        CriterionTitleWords criterionTitleWords = new CriterionTitleWords("OU",titleWords,titleWordsOperator);
        criteria.add(criterionTitleWords);

        List<NoticeV1Solr> newCandidates = noticeRepository.findNoticesByCriteria(criteria, sort, PageRequest.of(0,25));

        Assert.assertEquals(originalCandidates,newCandidates);
    }

    /**
     * Test de l'historiette #id215
     */
    @Test
    @DisplayName("historiette #id 215")
    void testId215() {
        Sort sort = new Sort(Sort.Direction.ASC, NoticeV1SolrField.PPN);

        // Requête de Periscope V1
        String originalQuery = "930-z_s:PCCor AND ((530-a_t:\"corse\"~5 OR 531-a_t:\"corse\"~5 OR 200-a_t:\"corse\"~5 OR 200-c_t:\"corse\"~5 OR 200-d_t:\"corse\"~5 OR 200-e_t:\"corse\"~5 OR 200-i_t:\"corse\"~5))";
        List<NoticeV1Solr> originalCandidates = noticeRepository.findNoticesBySolrQuery(originalQuery, sort, PageRequest.of(0,25));

        // Requête par Periscope V2
        List<Criterion> criteria = new LinkedList<>();
        List<String> pcp = Collections.singletonList("PCCor");
        List<String> pcpOperator = Collections.singletonList("ET");
        CriterionPcp criterionPcp = new CriterionPcp(pcp,pcpOperator);
        criteria.add(criterionPcp);

        List<String> titleWords = Collections.singletonList("corse");
        List<String> titleWordsOperator = Collections.singletonList("ET");
        CriterionTitleWords criterionTitleWords = new CriterionTitleWords("ET",titleWords,titleWordsOperator);
        criteria.add(criterionTitleWords);

        List<NoticeV1Solr> newCandidates = noticeRepository.findNoticesByCriteria(criteria, sort, PageRequest.of(0,25));

        Assert.assertEquals(originalCandidates,newCandidates);
    }

    /**
     * Test de l'historiette #id216
     */
    @Test
    @DisplayName("historiette #id 216")
    void testId216() {
        Sort sort = new Sort(Sort.Direction.ASC, NoticeV1SolrField.PPN);

        // Requête de Periscope V1
        String originalQuery = "930-z_s:PCCor AND NOT ((530-a_t:\"corse\"~5 OR 531-a_t:\"corse\"~5 OR 200-a_t:\"corse\"~5 OR 200-c_t:\"corse\"~5 OR 200-d_t:\"corse\"~5 OR 200-e_t:\"corse\"~5 OR 200-i_t:\"corse\"~5))";
        List<NoticeV1Solr> originalCandidates = noticeRepository.findNoticesBySolrQuery(originalQuery, sort, PageRequest.of(0,25));

        // Requête par Periscope V2
        List<Criterion> criteria = new LinkedList<>();
        List<String> pcp = Collections.singletonList("PCCor");
        List<String> pcpOperator = Collections.singletonList("ET");
        CriterionPcp criterionPcp = new CriterionPcp(pcp,pcpOperator);
        criteria.add(criterionPcp);

        List<String> titleWords = Collections.singletonList("corse");
        List<String> tileWordsOperator = Collections.singletonList("ET");
        CriterionTitleWords criterionTitleWords = new CriterionTitleWords("SAUF",titleWords,tileWordsOperator);
        criteria.add(criterionTitleWords);

        List<NoticeV1Solr> newCandidates = noticeRepository.findNoticesByCriteria(criteria, sort, PageRequest.of(0,25));

        Assert.assertEquals(originalCandidates,newCandidates);
    }

    @Test
    @DisplayName("historiette #id226")
    void testId226() {
        Sort sort = new Sort(Sort.Direction.ASC, NoticeV1SolrField.PPN);

        String originalQuery = "930-z_s:PCCor AND ((101-a_t:cos AND 530-a_t:[* TO *]))";
        List<NoticeV1Solr> originalCandidates = noticeRepository.findNoticesBySolrQuery(originalQuery, sort, PageRequest.of(0,25));

        List<Criterion> criteria = new LinkedList<>();

        List<String> pcp = Collections.singletonList("PCCor");
        List<String> pcpOperator = Collections.singletonList("OU");
        CriterionPcp criterionPcp = new CriterionPcp(pcp,pcpOperator);
        criteria.add(criterionPcp);

        List<String> language = Collections.singletonList("cos");
        List<String> operator = Collections.singletonList("ET");
        CriterionLanguage criterionLangue = new CriterionLanguage("ET",language,operator);
        criteria.add(criterionLangue);

        List<NoticeV1Solr> newCandidates = noticeRepository.findNoticesByCriteria(criteria, sort, PageRequest.of(0,25));

        Assert.assertEquals(originalCandidates,newCandidates);

    }

    @Test
    @DisplayName("historiette #id227")
    void testId227() {
        Sort sort = new Sort(Sort.Direction.ASC, NoticeV1SolrField.PPN);

        String originalQuery =  "930-z_s:PCCor OR (101-a_t:cos AND 530-a_t:[* TO *])";
        List<NoticeV1Solr> originalCandidates = noticeRepository.findNoticesBySolrQuery(originalQuery, sort, PageRequest.of(0,25));

        List<Criterion> criteria = new LinkedList<>();

        List<String> pcp = Collections.singletonList("PCCor");
        List<String> pcpOperator = Collections.singletonList("OU");
        CriterionPcp criterionPcp = new CriterionPcp(pcp,pcpOperator);
        criteria.add(criterionPcp);

        List<String> language = Collections.singletonList("cos");
        List<String> operator = Collections.singletonList("ET");
        CriterionLanguage criterionLanguage = new CriterionLanguage("OU",language,operator);
        criteria.add(criterionLanguage);

        List<NoticeV1Solr> newCandidates = noticeRepository.findNoticesByCriteria(criteria, sort, PageRequest.of(0,25));

        Assert.assertEquals(originalCandidates,newCandidates);
    }

    @Test
    @DisplayName("historiette #id228")
    void testId228() {
        Sort sort = new Sort(Sort.Direction.ASC, NoticeV1SolrField.PPN);

        String originalQuery = "930-z_s:PCCor AND -(101-a_t:cos AND 530-a_t:[* TO *])";
        List<NoticeV1Solr> originalCandidates = noticeRepository.findNoticesBySolrQuery(originalQuery, sort, PageRequest.of(0,25));
        List<Criterion> criteria = new LinkedList<>();

        List<String> pcp = Collections.singletonList("PCCor");
        List<String> pcpOperator = Collections.singletonList("OU");
        CriterionPcp criterionPcp = new CriterionPcp(pcp,pcpOperator);
        criteria.add(criterionPcp);

        List<String> language = Collections.singletonList("cos");
        List<String> operator = Collections.singletonList("ET");
        CriterionLanguage criterionLanguage = new CriterionLanguage("SAUF",language,operator);
        criteria.add(criterionLanguage);

        List<NoticeV1Solr> newCandidates = noticeRepository.findNoticesByCriteria(criteria, sort, PageRequest.of(0,25));

        Assert.assertEquals(originalCandidates,newCandidates);
    }

    /**
     * Test de l'historiette #id218
     */
    @Test
    @DisplayName("historiette #id 218")
    void testId218() {
        Sort sort = new Sort(Sort.Direction.ASC, NoticeV1SolrField.PPN);

        // Requête de Periscope V1
        String originalQuery = "930-z_s:PCCor AND (210-c_t:\"corse\")";
        List<NoticeV1Solr> originalCandidates = noticeRepository.findNoticesBySolrQuery(originalQuery, sort, PageRequest.of(0,25));

        // Requête par Periscope V2
        List<Criterion> criteria = new LinkedList<>();
        List<String> pcp = Collections.singletonList("PCCor");
        List<String> pcpOperator = Collections.singletonList("ET");
        CriterionPcp criterionPcp = new CriterionPcp(pcp,pcpOperator);
        criteria.add(criterionPcp);

        List<String> editors = Collections.singletonList("corse");
        List<String> editorsOperator = Collections.singletonList("ET");
        CriterionEditor criterionEditor = new CriterionEditor("ET",editors,editorsOperator);
        criteria.add(criterionEditor);

        List<NoticeV1Solr> newCandidates = noticeRepository.findNoticesByCriteria(criteria, sort, PageRequest.of(0,25));

        Assert.assertEquals(originalCandidates,newCandidates);
    }

    /**
     * Test de l'historiette #id219
     */
    @Test
    @DisplayName("historiette #id 219")
    void testId219() {
        Sort sort = new Sort(Sort.Direction.ASC, NoticeV1SolrField.PPN);

        // Requête de Periscope V1
        String originalQuery = "930-z_s:PCCor OR (210-c_t:\"corse\")";
        List<NoticeV1Solr> originalCandidates = noticeRepository.findNoticesBySolrQuery(originalQuery, sort, PageRequest.of(0,25));

        // Requête par Periscope V2
        List<Criterion> criteria = new LinkedList<>();
        List<String> pcp = Collections.singletonList("PCCor");
        List<String> pcpOperator = Collections.singletonList("ET");
        CriterionPcp criterionPcp = new CriterionPcp(pcp,pcpOperator);
        criteria.add(criterionPcp);

        List<String> editors = Collections.singletonList("corse");
        List<String> editorsOperator = Collections.singletonList("ET");
        CriterionEditor criterionEditor = new CriterionEditor("OU",editors,editorsOperator);
        criteria.add(criterionEditor);

        List<NoticeV1Solr> newCandidates = noticeRepository.findNoticesByCriteria(criteria, sort, PageRequest.of(0,25));

        Assert.assertEquals(originalCandidates,newCandidates);
    }

    /**
     * Test de l'historiette #id220
     */
    @Test
    @DisplayName("historiette #id 220")
    void testId220() {
        Sort sort = new Sort(Sort.Direction.ASC, NoticeV1SolrField.PPN);

        // Requête de Periscope V1
        String originalQuery = "930-z_s:PCCor AND NOT (210-c_t:\"corse\")";
        List<NoticeV1Solr> originalCandidates = noticeRepository.findNoticesBySolrQuery(originalQuery, sort, PageRequest.of(0,25));

        // Requête par Periscope V2
        List<Criterion> criteria = new LinkedList<>();
        List<String> pcp = Collections.singletonList("PCCor");
        List<String> pcpOperator = Collections.singletonList("ET");
        CriterionPcp criterionPcp = new CriterionPcp(pcp,pcpOperator);
        criteria.add(criterionPcp);

        List<String> editors = Collections.singletonList("corse");
        List<String> editorsOperator = Collections.singletonList("ET");
        CriterionEditor criterionEditor = new CriterionEditor("SAUF",editors,editorsOperator);
        criteria.add(criterionEditor);

        List<NoticeV1Solr> newCandidates = noticeRepository.findNoticesByCriteria(criteria, sort, PageRequest.of(0,25));

        Assert.assertEquals(originalCandidates,newCandidates);
    }

    /**
     * Test de l'historiette #id222
     */
    @Test
    @DisplayName("historiette #id 222")
    void testId222() {
        Sort sort = new Sort(Sort.Direction.ASC, NoticeV1SolrField.PPN);

        // Requête de Periscope V1
        String originalQuery = "930-b_t:200962101 AND (210-c_t:\"corse\")";
        List<NoticeV1Solr> originalCandidates = noticeRepository.findNoticesBySolrQuery(originalQuery, sort, PageRequest.of(0,25));

        // Requête par Periscope V2
        List<Criterion> criteria = new LinkedList<>();
        List<String> rcr = Collections.singletonList("200962101");
        List<String> rcrOperator = Collections.singletonList("ET");
        CriterionRcr criterionRcr = new CriterionRcr(rcr,rcrOperator);
        criteria.add(criterionRcr);

        List<String> editors = Collections.singletonList("corse");
        List<String> editorsOperator = Collections.singletonList("ET");
        CriterionEditor criterionEditor = new CriterionEditor("ET",editors,editorsOperator);
        criteria.add(criterionEditor);

        List<NoticeV1Solr> newCandidates = noticeRepository.findNoticesByCriteria(criteria, sort, PageRequest.of(0,25));

        Assert.assertEquals(originalCandidates,newCandidates);
    }

    /**
     * Test de l'historiette #id223
     */
    @Test
    @DisplayName("historiette #id 223")
    void testId223() {
        Sort sort = new Sort(Sort.Direction.ASC, NoticeV1SolrField.PPN);

        // Requête de Periscope V1
        String originalQuery = "930-b_t:200962101 OR (210-c_t:\"corse\")";
        List<NoticeV1Solr> originalCandidates = noticeRepository.findNoticesBySolrQuery(originalQuery, sort, PageRequest.of(0,25));

        // Requête par Periscope V2
        List<Criterion> criteria = new LinkedList<>();
        List<String> rcr = Collections.singletonList("200962101");
        List<String> rcrOperator = Collections.singletonList("ET");
        CriterionRcr criterionRcr = new CriterionRcr(rcr,rcrOperator);
        criteria.add(criterionRcr);

        List<String> editors = Collections.singletonList("corse");
        List<String> editorsOperator = Collections.singletonList("ET");
        CriterionEditor criterionEditor = new CriterionEditor("OU",editors,editorsOperator);
        criteria.add(criterionEditor);

        List<NoticeV1Solr> newCandidates = noticeRepository.findNoticesByCriteria(criteria, sort, PageRequest.of(0,25));

        Assert.assertEquals(originalCandidates,newCandidates);
    }

    /*
     * Test de l'historiette #id224
     */
    @Test
    @DisplayName("historiette #id 224")
    void testId224() {
        Sort sort = new Sort(Sort.Direction.ASC, NoticeV1SolrField.PPN);

        // Requête de Periscope V1
        String originalQuery = "930-b_t:200962101 AND NOT (210-c_t:\"corse\")";
        List<NoticeV1Solr> originalCandidates = noticeRepository.findNoticesBySolrQuery(originalQuery, sort, PageRequest.of(0,25));

        // Requête par Periscope V2
        List<Criterion> criteria = new LinkedList<>();
        List<String> rcr = Collections.singletonList("200962101");
        List<String> rcrOperator = Collections.singletonList("ET");
        CriterionRcr criterionRcr = new CriterionRcr(rcr,rcrOperator);
        criteria.add(criterionRcr);

        List<String> editors = Collections.singletonList("corse");
        List<String> editorsOperator = Collections.singletonList("ET");
        CriterionEditor criterionEditor = new CriterionEditor("SAUF",editors,editorsOperator);
        criteria.add(criterionEditor);

        List<NoticeV1Solr> newCandidates = noticeRepository.findNoticesByCriteria(criteria, sort, PageRequest.of(0,25));

        assertEquals(originalCandidates,newCandidates);
    }

    /**
     * Test de l'historiette #id230
     */
    @Test
    @DisplayName("historiette #id 230")
    void testId230() {
        Sort sort = new Sort(Sort.Direction.ASC, NoticeV1SolrField.PPN);

        // Requête de Periscope V1
        String originalQuery = "930-b_t:200962101 AND (102-a_t:IT AND 530-a_t:[* TO *]) ";
        List<NoticeV1Solr> originalCandidates = noticeRepository.findNoticesBySolrQuery(originalQuery, sort, PageRequest.of(0,25));

        // Requête par Periscope V2
        List<Criterion> criteria = new LinkedList<>();
        List<String> rcr = Collections.singletonList("200962101");
        List<String> rcrOperator = Collections.singletonList("ET");
        CriterionRcr criterionRcr = new CriterionRcr(rcr,rcrOperator);
        criteria.add(criterionRcr);

        List<String> countries = Collections.singletonList("IT");
        List<String> countriesOperator = Collections.singletonList("ET");
        CriterionCountry criterionCountry = new CriterionCountry("ET",countries,countriesOperator);
        criteria.add(criterionCountry);

        List<NoticeV1Solr> newCandidates = noticeRepository.findNoticesByCriteria(criteria, sort, PageRequest.of(0,25));

        assertEquals(originalCandidates,newCandidates);
    }

    /**
     * Test de l'historiette #id231
     */
    @Test
    @DisplayName("historiette #id 231")
    void testId231() {
        Sort sort = new Sort(Sort.Direction.ASC, NoticeV1SolrField.PPN);

        // Requête de Periscope V1
        String originalQuery = "930-b_t:200962101 OR (102-a_t:IT AND 530-a_t:[* TO *]) ";
        List<NoticeV1Solr> originalCandidates = noticeRepository.findNoticesBySolrQuery(originalQuery, sort, PageRequest.of(0,25));

        // Requête par Periscope V2
        List<Criterion> criteria = new LinkedList<>();
        List<String> rcr = Collections.singletonList("200962101");
        List<String> rcrOperator = Collections.singletonList("ET");
        CriterionRcr criterionRcr = new CriterionRcr(rcr,rcrOperator);
        criteria.add(criterionRcr);

        List<String> countries = Collections.singletonList("IT");
        List<String> countriesOperator = Collections.singletonList("ET");
        CriterionCountry criterionCountry = new CriterionCountry("OU",countries,countriesOperator);
        criteria.add(criterionCountry);

        List<NoticeV1Solr> newCandidates = noticeRepository.findNoticesByCriteria(criteria, sort, PageRequest.of(0,25));

        assertEquals(originalCandidates,newCandidates);
    }

    /**
     * Test de l'historiette #id232
     */
    @Test
    @DisplayName("historiette #id 232")
    void testId232() {
        Sort sort = new Sort(Sort.Direction.ASC, NoticeV1SolrField.PPN);

        // Requête de Periscope V1
        String originalQuery = "930-b_t:200962101 AND NOT (102-a_t:IT AND 530-a_t:[* TO *]) ";
        List<NoticeV1Solr> originalCandidates = noticeRepository.findNoticesBySolrQuery(originalQuery, sort, PageRequest.of(0,25));

        // Requête par Periscope V2
        List<Criterion> criteria = new LinkedList<>();
        List<String> rcr = Collections.singletonList("200962101");
        List<String> rcrOperator = Collections.singletonList("ET");
        CriterionRcr criterionRcr = new CriterionRcr(rcr,rcrOperator);
        criteria.add(criterionRcr);

        List<String> countries = Collections.singletonList("IT");
        List<String> countriesOperator = Collections.singletonList("ET");
        CriterionCountry criterionCountry = new CriterionCountry("SAUF",countries,countriesOperator);
        criteria.add(criterionCountry);

        List<NoticeV1Solr> newCandidates = noticeRepository.findNoticesByCriteria(criteria, sort, PageRequest.of(0,25));

        assertEquals(originalCandidates,newCandidates);
    }

    /**
     * Test de l'historiette #idX1
     */
    @Test
    @DisplayName("historiette #id X1")
    void testIdX1() {
        Sort sort = new Sort(Sort.Direction.ASC, NoticeV1SolrField.PPN);

        // Requête de Periscope V1
        String originalQuery = "((930-z_s:PCAq OR 930-z_s:PCAuv OR 930-z_s:PCBo OR 930-z_s:PCBre OR 930-z_s:PCCA OR 930-z_s:PCCAPI OR 930-z_s:PCCor OR 930-z_s:PCFC OR 930-z_s:PCLR OR 930-z_s:PCLim OR 930-z_s:PCLor OR 930-z_s:PCMP OR 930-z_s:PCNPDC OR 930-z_s:PCPACA OR 930-z_s:PCPCh OR 930-z_s:PCPL OR 930-z_s:PCPic OR 930-z_s:PCRA OR 930-z_s:PCSAM OR 930-z_s:PCSCen OR 930-z_s:PCUP OR 930-z_s:PCUR OR 930-z_s:PCAM OR 930-z_s:PCAS OR 930-z_s:PCAnt OR 930-z_s:PCChimie OR 930-z_s:PCDroit OR 930-z_s:PCEBCO OR 930-z_s:PCGer OR 930-z_s:PCGéo OR 930-z_s:PCIta OR 930-z_s:PCMath OR 930-z_s:PCMed OR 930-z_s:PCMedieval OR 930-z_s:PCNum OR 930-z_s:PCPhilo OR 930-z_s:PCPhy OR 930-z_s:PCPsy OR 930-z_s:PCSTAPS) AND NOT 930-b_t:751052105)";
        List<NoticeV1Solr> originalCandidates = noticeRepository.findNoticesBySolrQuery(originalQuery, sort, PageRequest.of(0,25));

        // Requête par Periscope V2
        List<Criterion> criteria = new LinkedList<>();

        List<String> pcp = Arrays.asList("PCAq","PCAuv","PCBo","PCBre","PCCA","PCCAPI","PCCor","PCFC","PCLR","PCLim","PCLor","PCMP","PCNPDC","PCPACA","PCPCh","PCPL","PCPic","PCRA","PCSAM","PCSCen","PCUP","PCUR","PCAM","PCAS","PCAnt","PCChimie","PCDroit","PCEBCO","PCGer","PCGéo","PCIta","PCMath","PCMed","PCMedieval","PCNum","PCPhilo","PCPhy", "PCPsy", "PCSTAPS");
        List<String> pcpOperator = Arrays.asList("OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU");
        CriterionPcp criterionPcp = new CriterionPcp(pcp,pcpOperator);
        criteria.add(criterionPcp);

        List<String> rcr = Collections.singletonList("751052105");
        List<String> operator = Collections.singletonList("OU");
        CriterionRcr criterionRcr = new CriterionRcr("SAUF",rcr,operator);
        criteria.add(criterionRcr);

        List<NoticeV1Solr> newCandidates = noticeRepository.findNoticesByCriteria(criteria, sort, PageRequest.of(0,25));

        assertEquals(originalCandidates,newCandidates);
    }

    /**
     * Test de l'historiette #idX2
     */
    @Test
    @DisplayName("historiette #id X2")
    void testIdX2() {
        Sort sort = new Sort(Sort.Direction.ASC, NoticeV1SolrField.PPN);

        // Requête de Periscope V1
        //String originalQuery = "( pcprcr_t:674821001PCAq OR pcprcr_t:674821001PCAuv OR pcprcr_t:674821001PCBo OR pcprcr_t:674821001PCBre OR pcprcr_t:674821001PCCA OR pcprcr_t:674821001PCCAPI OR pcprcr_t:674821001PCCor OR pcprcr_t:674821001PCFC OR pcprcr_t:674821001PCLR OR pcprcr_t:674821001PCLim OR pcprcr_t:674821001PCLor OR pcprcr_t:674821001PCMP OR pcprcr_t:674821001PCNPDC OR pcprcr_t:674821001PCPACA OR pcprcr_t:674821001PCPCh OR pcprcr_t:674821001PCPL OR pcprcr_t:674821001PCPic OR pcprcr_t:674821001PCRA OR pcprcr_t:674821001PCSAM OR pcprcr_t:674821001PCSCen OR pcprcr_t:674821001PCUP OR pcprcr_t:674821001PCUR OR pcprcr_t:674821001PCAM OR pcprcr_t:674821001PCAS OR pcprcr_t:674821001PCAnt OR pcprcr_t:674821001PCChimie OR pcprcr_t:674821001PCDroit OR pcprcr_t:674821001PCEBCO OR pcprcr_t:674821001PCGer OR pcprcr_t:674821001PCGéo OR pcprcr_t:674821001PCIta OR pcprcr_t:674821001PCMath OR pcprcr_t:674821001PCMed OR pcprcr_t:674821001PCMedieval OR pcprcr_t:674821001PCNum OR pcprcr_t:674821001PCPhilo OR pcprcr_t:674821001PCPhy OR pcprcr_t:674821001PCPsy OR pcprcr_t:674821001PCSTAPS)";
        // Requête comme Solr total
        String originalQuery = "( 930-z_s:PCAq OR  930-z_s:PCAuv OR  930-z_s:PCBo OR  930-z_s:PCBre OR  930-z_s:PCCA OR  930-z_s:PCCAPI OR  930-z_s:PCCor OR  930-z_s:PCFC OR  930-z_s:PCLR OR  930-z_s:PCLim OR  930-z_s:PCLor OR  930-z_s:PCMP OR  930-z_s:PCNPDC OR  930-z_s:PCPACA OR  930-z_s:PCPCh OR  930-z_s:PCPL OR  930-z_s:PCPic OR  930-z_s:PCRA OR  930-z_s:PCSAM OR  930-z_s:PCSCen OR  930-z_s:PCUP OR  930-z_s:PCUR OR  930-z_s:PCAM OR  930-z_s:PCAS OR  930-z_s:PCAnt OR  930-z_s:PCChimie OR  930-z_s:PCDroit OR  930-z_s:PCEBCO OR  930-z_s:PCGer OR  930-z_s:PCGéo OR  930-z_s:PCIta OR  930-z_s:PCMath OR  930-z_s:PCMed OR  930-z_s:PCMedieval OR  930-z_s:PCNum OR  930-z_s:PCPhilo OR  930-z_s:PCPhy OR  930-z_s:PCPsy OR  930-z_s:PCSTAPS) AND 930-b_t:674821001";

        List<NoticeV1Solr> originalCandidates = noticeRepository.findNoticesBySolrQuery(originalQuery, sort, PageRequest.of(0,25));

        // Requête par Periscope V2
        List<Criterion> criteria = new LinkedList<>();

        List<String> pcp = Arrays.asList("PCAq","PCAuv","PCBo","PCBre","PCCA","PCCAPI","PCCor","PCFC","PCLR","PCLim","PCLor","PCMP","PCNPDC","PCPACA","PCPCh","PCPL","PCPic","PCRA","PCSAM","PCSCen","PCUP","PCUR","PCAM","PCAS","PCAnt","PCChimie","PCDroit","PCEBCO","PCGer","PCGéo","PCIta","PCMath","PCMed","PCMedieval","PCNum","PCPhilo","PCPhy", "PCPsy", "PCSTAPS");
        List<String> pcpOperator = Arrays.asList("OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU","OU");
        CriterionPcp criterionPcp = new CriterionPcp(pcp,pcpOperator);
        criteria.add(criterionPcp);

        List<String> rcr = Collections.singletonList("674821001");
        List<String> rcrOperator = Collections.singletonList("ET");
        CriterionRcr criterionRcr = new CriterionRcr("ET",rcr,rcrOperator);
        criteria.add(criterionRcr);

        List<NoticeV1Solr> newCandidates = noticeRepository.findNoticesByCriteria(criteria, sort, PageRequest.of(0,25));

        assertEquals(originalCandidates,newCandidates);
    }

    /**
     * Test de l'historiette #idX3
     */
    @Test
    @DisplayName("historiette #id X3")
    void testIdX3() {
        Sort sort = new Sort(Sort.Direction.ASC, NoticeV1SolrField.PPN);

        // Requête de Periscope V1
        String originalQuery = "(930-z_s:PCDroit OR (930-b_t:212312101 OR 930-b_t:341722102))";
        List<NoticeV1Solr> originalCandidates = noticeRepository.findNoticesBySolrQuery(originalQuery, sort, PageRequest.of(0,25));

        // Requête par Periscope V2
        List<Criterion> criteria = new LinkedList<>();

        List<String> pcp = Collections.singletonList("PCDroit");
        List<String> pcpOperator = Collections.singletonList("OU");
        CriterionPcp criterionPcp = new CriterionPcp(pcp,pcpOperator);
        criteria.add(criterionPcp);

        List<String> rcr = Arrays.asList("212312101","341722102");
        List<String> rcrOperator = Arrays.asList("ET","OU");
        CriterionRcr criterionRcr = new CriterionRcr("OU",rcr,rcrOperator);
        criteria.add(criterionRcr);

        List<NoticeV1Solr> newCandidates = noticeRepository.findNoticesByCriteria(criteria, sort, PageRequest.of(0,25));

        assertEquals(originalCandidates,newCandidates);
    }

    /**
     * Test de l'historiette #idX4
     */
    @Test
    @DisplayName("historiette #id X4")
    void testIdX4() {
        Sort sort = new Sort(Sort.Direction.ASC, NoticeV1SolrField.PPN);

        String originalQuery = "((930-z_s:PCDroit OR 930-z_s:PCPhilo) OR (930-b_t:212312101 OR 930-b_t:341722102))";
        List<NoticeV1Solr> originalCandidates = noticeRepository.findNoticesBySolrQuery(originalQuery, sort, PageRequest.of(0,25));

        List<Criterion> criteria = new LinkedList<>();

        List<String> pcp = Arrays.asList("PCDroit","PCPhilo");
        List<String> pcpOperator = Arrays.asList("OU","OU");
        CriterionPcp criterionPcp = new CriterionPcp(pcp,pcpOperator);
        criteria.add(criterionPcp);

        List<String> rcr = Arrays.asList("212312101","341722102");
        List<String> rcrOperator = Arrays.asList("ET","OU");
        CriterionRcr criterionRcr = new CriterionRcr("OU",rcr,rcrOperator);
        criteria.add(criterionRcr);

        List<NoticeV1Solr> newCandidates = noticeRepository.findNoticesByCriteria(criteria, sort, PageRequest.of(0,25));

        assertEquals(originalCandidates,newCandidates);
    }

    @Test
    @DisplayName("Test Critère ISSN")
    void testIssn() {
        Sort sort = new Sort(Sort.Direction.ASC, NoticeV1SolrField.PPN);

        String originalQuery = "011-a_t:1146-7665";
        List<NoticeV1Solr> originalCandidates = noticeRepository.findNoticesBySolrQuery(originalQuery, sort, PageRequest.of(0, 25, Sort.by(Sort.Direction.ASC, NoticeV1SolrField.PPN)));

        List<Criterion> criteria = new LinkedList<>();
        List<String> issn = Collections.singletonList("1146-7665");
        CriterionIssn criterionIssn = new CriterionIssn(issn);
        criteria.add(criterionIssn);

        List<NoticeV1Solr> newCandidates = noticeRepository.findNoticesByCriteria(criteria, sort, PageRequest.of(0,25));

        assertEquals(originalCandidates, newCandidates);
    }

    @Test
    @DisplayName("Test requête avec critère de tri")
    void testSortOrder1() {
        Sort sort = new Sort(Sort.Direction.DESC, NoticeV1SolrField.KEY_TITLE);

        String originalQuery = "((930-z_s:PCDroit OR 930-z_s:PCPhilo) OR (930-b_t:212312101 OR 930-b_t:341722102))";
        List<NoticeV1Solr> originalCandidates = noticeRepository.findNoticesBySolrQuery(originalQuery, sort, PageRequest.of(0,25));

        List<Criterion> criteria = new LinkedList<>();

        List<String> pcp = Arrays.asList("PCDroit","PCPhilo");
        List<String> pcpOperator = Arrays.asList("OU","OU");
        CriterionPcp criterionPcp = new CriterionPcp(pcp,pcpOperator);
        criteria.add(criterionPcp);

        List<String> rcr = Arrays.asList("212312101","341722102");
        List<String> rcrOperator = Arrays.asList("ET","OU");
        CriterionRcr criterionRcr = new CriterionRcr("OU",rcr,rcrOperator);
        criteria.add(criterionRcr);

        List<NoticeV1Solr> newCandidates = noticeRepository.findNoticesByCriteria(criteria, sort, PageRequest.of(0,25));
        assertEquals(originalCandidates, newCandidates);

    }

    @Test
    @DisplayName("Test requête avec critère de tri multiples")
    void testSortOrder2() {
        Sort sort = new Sort(Sort.Direction.ASC, NoticeV1SolrField.CONTINIOUS_TYPE);
        sort.and(new Sort(Sort.Direction.DESC, NoticeV1SolrField.KEY_TITLE));

        String originalQuery = "((930-z_s:PCDroit OR 930-z_s:PCPhilo) OR (930-b_t:212312101 OR 930-b_t:341722102))";
        List<NoticeV1Solr> originalCandidates = noticeRepository.findNoticesBySolrQuery(originalQuery, sort, PageRequest.of(0,25));

        List<Criterion> criteria = new LinkedList<>();

        List<String> pcp = Arrays.asList("PCDroit","PCPhilo");
        List<String> pcpOperator = Arrays.asList("OU","OU");
        CriterionPcp criterionPcp = new CriterionPcp(pcp,pcpOperator);
        criteria.add(criterionPcp);

        List<String> rcr = Arrays.asList("212312101","341722102");
        List<String> rcrOperator = Arrays.asList("ET","OU");
        CriterionRcr criterionRcr = new CriterionRcr("OU",rcr,rcrOperator);
        criteria.add(criterionRcr);


        List<NoticeV1Solr> newCandidates = noticeRepository.findNoticesByCriteria(criteria, sort, PageRequest.of(0,25));
        assertEquals(originalCandidates, newCandidates);

    }
}
