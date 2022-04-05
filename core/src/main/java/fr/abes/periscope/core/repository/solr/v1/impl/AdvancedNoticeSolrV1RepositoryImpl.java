package fr.abes.periscope.core.repository.solr.v1.impl;

import fr.abes.periscope.core.criterion.Criterion;
import fr.abes.periscope.core.entity.solr.v1.NoticeV1Solr;
import fr.abes.periscope.core.entity.solr.v1.NoticeV1SolrField;
import fr.abes.periscope.core.repository.solr.v1.AdvancedNoticeSolrV1Repository;
import fr.abes.periscope.core.repository.solr.v1.SolrQueryBuilder;
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
@Deprecated
@Data
@Slf4j
@Repository
public class AdvancedNoticeSolrV1RepositoryImpl implements AdvancedNoticeSolrV1Repository {

    private final SolrTemplate solrTemplate;

    private final SolrQueryBuilder builderQuery;

    /** Core du serveur SolR */
    @Value("${periscope.solr.v1.core}")
    private String core;

    public AdvancedNoticeSolrV1RepositoryImpl(@Qualifier("solrV1Template") SolrTemplate template, @Qualifier("SolrQueryV1Builder") SolrQueryBuilder builder) {
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
     * @return List<NoticeV1Solr> Liste de Notices SolR
     */
    @Override
    public List<NoticeV1Solr> findNoticesByCriteria(List<Criterion> criteria, Sort sort, Pageable page) {

        SimpleQuery solrQuery = new SimpleQuery(builderQuery.buildQuery(criteria),page);
        solrQuery.addProjectionOnFields(
                NoticeV1SolrField.PPN,
                NoticeV1SolrField.ISSN_T,
                NoticeV1SolrField.PCP_T,
                NoticeV1SolrField.RCR_T,
                NoticeV1SolrField.EDITOR,
                NoticeV1SolrField.PROCESSING_GLOBAL_DATA,
                NoticeV1SolrField.KEY_TITLE,
                NoticeV1SolrField.ISSN,
                NoticeV1SolrField.KEY_SHORTED_TITLE,
                NoticeV1SolrField.PROPER_TITLE,
                NoticeV1SolrField.TITLE_FROM_DIFFERENT_AUTHOR,
                NoticeV1SolrField.PARALLEL_TITLE,
                NoticeV1SolrField.TITLE_COMPLEMENT,
                NoticeV1SolrField.SECTION_TITLE,
                NoticeV1SolrField.KEY_TITLE_QUALIFIER,
                NoticeV1SolrField.CONTINIOUS_TYPE,
                NoticeV1SolrField.EXTERNAL_URLS_S,
                NoticeV1SolrField.NB_LOC);
        solrQuery.addSort(sort);
        // Debug query
        DefaultQueryParser dqp = new DefaultQueryParser(null);
        String actualQuery = dqp.getQueryString(solrQuery, null);
        log.debug("SolR query : "+actualQuery);

        Page results = solrTemplate.queryForPage(core,solrQuery, NoticeV1Solr.class);
        return results.getContent();
    }

    /**
     * Retourne les Notices SolR selon une requête SolR, un critère de tri
     * et une page.
     * La liste des champs SolR a récupéré est inscrites en dur dans la méthode
     * @param query Requête SolR
     * @param sort Les critères de tri
     * @param page La page souhaitée
     * @return List<NoticeV1Solr> Liste de Notices SolR
     */
    public List<NoticeV1Solr> findNoticesBySolrQuery(String query, Sort sort, Pageable page) {

        SimpleQuery solrQuery = new SimpleQuery(query,page);
        solrQuery.addProjectionOnFields(
                NoticeV1SolrField.PPN,
                NoticeV1SolrField.ISSN_T,
                NoticeV1SolrField.PCP_T,
                NoticeV1SolrField.RCR_T,
                NoticeV1SolrField.EDITOR,
                NoticeV1SolrField.PROCESSING_GLOBAL_DATA,
                NoticeV1SolrField.KEY_TITLE,
                NoticeV1SolrField.ISSN,
                NoticeV1SolrField.KEY_SHORTED_TITLE,
                NoticeV1SolrField.PROPER_TITLE,
                NoticeV1SolrField.TITLE_FROM_DIFFERENT_AUTHOR,
                NoticeV1SolrField.PARALLEL_TITLE,
                NoticeV1SolrField.TITLE_COMPLEMENT,
                NoticeV1SolrField.SECTION_TITLE,
                NoticeV1SolrField.KEY_TITLE_QUALIFIER,
                NoticeV1SolrField.CONTINIOUS_TYPE);
        solrQuery.addSort(sort);
        // Debug query
        DefaultQueryParser dqp = new DefaultQueryParser(null);
        String actualQuery = dqp.getQueryString(solrQuery, null);
        log.debug("SolR query : "+actualQuery);

        Page results = solrTemplate.queryForPage(core,solrQuery, NoticeV1Solr.class);
        return results.getContent();
    }
}
