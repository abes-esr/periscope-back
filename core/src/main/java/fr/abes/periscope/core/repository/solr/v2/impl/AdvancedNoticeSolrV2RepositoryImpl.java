package fr.abes.periscope.core.repository.solr.v2.impl;

import fr.abes.periscope.core.criterion.Criterion;
import fr.abes.periscope.core.entity.v2.solr.NoticeV2SolrField;
import fr.abes.periscope.core.entity.v2.solr.NoticeV2Solr;
import fr.abes.periscope.core.repository.solr.v2.AdvancedNoticeSolrV2Repository;
import fr.abes.periscope.core.repository.solr.v2.SolrQueryBuilder;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.solr.core.DefaultQueryParser;
import org.springframework.data.solr.core.SolrTemplate;
import org.springframework.data.solr.core.query.SimpleQuery;
import org.springframework.stereotype.Repository;

import java.util.List;

/**
 * Représente un dépôt de Notice SolR avec des requêtes complexes
 */
@Data
@Slf4j
@Repository
public class AdvancedNoticeSolrV2RepositoryImpl implements AdvancedNoticeSolrV2Repository {

    private final SolrTemplate solrTemplate;

    private final SolrQueryBuilder builderQuery;

    @Value("${solr.v2.core}")
    private String core;

    public AdvancedNoticeSolrV2RepositoryImpl(@Qualifier("solrV2Template") SolrTemplate template, @Qualifier("SolrQueryV2Builder") SolrQueryBuilder builder) {
        solrTemplate = template;
        builderQuery = builder;
    }

    /**
     * Retourne les Notices SolR selon une liste de critères, un critère de tri
     * et une page.
     * La liste des champs SolR a récupéré est inscrites en dur dans la méthode
     * @param criteria Les critères de la recherche
     * @param sort Les critères de tri
     * @param page La page souhaitée
     * @return List<NoticeV2Solr> Liste de Notices SolR
     */
    @Override
    public List<NoticeV2Solr> findNoticesByCriteria(List<Criterion> criteria, Sort sort, Pageable page) {

        SimpleQuery solrQuery = new SimpleQuery(builderQuery.buildQuery(criteria),page);
        solrQuery.addProjectionOnFields(
                NoticeV2SolrField.PPN,
                /*NoticeV2SolrField.ISSN,
                NoticeV2SolrField.PCP_LIST,
                NoticeV2SolrField.RCR_LIST,
                NoticeV2SolrField.EDITOR,
                NoticeV2SolrField.PROCESSING_GLOBAL_DATA,
                NoticeV2SolrField.KEY_TITLE,
                NoticeV2SolrField.ISSN,
                NoticeV2SolrField.KEY_SHORTED_TITLE,*/
                NoticeV2SolrField.PROPER_TITLE,/*
                NoticeV2SolrField.TITLE_FROM_DIFFERENT_AUTHOR,
                NoticeV2SolrField.PARALLEL_TITLE,
                NoticeV2SolrField.TITLE_COMPLEMENT,
                NoticeV2SolrField.SECTION_TITLE,
                NoticeV2SolrField.KEY_TITLE_QUALIFIER,*/
                NoticeV2SolrField.TYPE_DOCUMENT/*,
                NoticeV2SolrField.EXTERNAL_URLS,
                NoticeV2SolrField.NB_LOC*/);
        solrQuery.addSort(sort);
        // Debug query
        DefaultQueryParser dqp = new DefaultQueryParser(null);
        String actualQuery = dqp.getQueryString(solrQuery, null);
        log.debug("SolR query : "+actualQuery);

        Page results = solrTemplate.queryForPage(core,solrQuery, NoticeV2Solr.class);
        return results.getContent();
    }

    /**
     * Retourne les Notices SolR selon une requête SolR, un critère de tri
     * et une page.
     * La liste des champs SolR a récupéré est inscrites en dur dans la méthode
     * @param query Requête SolR
     * @param sort Les critères de tri
     * @param page La page souhaitée
     * @return List<NoticeV2Solr> Liste de Notices SolR
     */
    public List<NoticeV2Solr> findNoticesBySolrQuery(String query, Sort sort, Pageable page) {

        SimpleQuery solrQuery = new SimpleQuery(query,page);
        solrQuery.addProjectionOnFields(
                NoticeV2SolrField.PPN,
                /*NoticeV2SolrField.ISSN,
                NoticeV2SolrField.PCP_LIST,
                NoticeV2SolrField.RCR_LIST,
                NoticeV2SolrField.EDITOR,
                NoticeV2SolrField.PROCESSING_GLOBAL_DATA,
                NoticeV2SolrField.KEY_TITLE,
                NoticeV2SolrField.ISSN,
                NoticeV2SolrField.KEY_SHORTED_TITLE,*/
                NoticeV2SolrField.PROPER_TITLE/*,
                NoticeV2SolrField.TITLE_FROM_DIFFERENT_AUTHOR,
                NoticeV2SolrField.PARALLEL_TITLE,
                NoticeV2SolrField.TITLE_COMPLEMENT,
                NoticeV2SolrField.SECTION_TITLE,
                NoticeV2SolrField.KEY_TITLE_QUALIFIER*/,
                NoticeV2SolrField.TYPE_DOCUMENT);
        solrQuery.addSort(sort);
        // Debug query
        DefaultQueryParser dqp = new DefaultQueryParser(null);
        String actualQuery = dqp.getQueryString(solrQuery, null);
        log.debug("SolR query : "+actualQuery);

        Page results = solrTemplate.queryForPage(core,solrQuery, NoticeV2Solr.class);
        return results.getContent();
    }
}
