package fr.abes.periscope.core.repository.solr.v1.impl;

import fr.abes.periscope.core.criterion.Criterion;
import fr.abes.periscope.core.entity.NoticeSolr;
import fr.abes.periscope.core.entity.v1.solr.NoticeV1Solr;
import fr.abes.periscope.core.entity.v1.solr.NoticeV1SolrField;
import fr.abes.periscope.core.repository.solr.v1.AdvancedNoticeRepository;
import fr.abes.periscope.core.repository.solr.v1.SolrV1QueryBuilder;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
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
public class AdvancedNoticeV1RepositoryImpl implements AdvancedNoticeRepository {

    @Autowired
    @Qualifier("solr-v1")
    private final SolrTemplate solrTemplate;

    private final SolrV1QueryBuilder builderQuery;

    @Override
    /**
     * Retourne les Notices SolR selon une liste de critères et une page
     * @param criteria Les critères de la recherche
     * @param page La page souhaitée
     * @return List<NoticeSolr> Liste de Notices SolR
     */
    public List<NoticeSolr> findNoticesByCriteria(List<Criterion> criteria, Sort sort, Pageable page) {

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

        Page results = solrTemplate.queryForPage("notice",solrQuery, NoticeV1Solr.class);
        return results.getContent();
    }

    public List<NoticeSolr> findNoticesBySolrQuery(String query, Sort sort, Pageable page) {

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

        Page results = solrTemplate.queryForPage("notice",solrQuery, NoticeV1Solr.class);
        return results.getContent();
    }
}
