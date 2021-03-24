package fr.abes.periscope.core.repository.solr.impl;

import fr.abes.periscope.core.criterion.Criterion;
import fr.abes.periscope.core.entity.NoticeSolrV1;
import fr.abes.periscope.core.repository.solr.AdvancedNoticeRepository;
import fr.abes.periscope.core.entity.NoticeV1Field;
import fr.abes.periscope.core.repository.solr.SolrQueryBuilder;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
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
public class AdvancedNoticeRepositoryImpl implements AdvancedNoticeRepository {

    private final SolrTemplate solrTemplate;

    private final SolrQueryBuilder builderQuery;

    @Override
    /**
     * Retourne les Notices SolR selon une liste de critères et une page
     * @param criteria Les critères de la recherche
     * @param page La page souhaitée
     * @return List<NoticeSolr> Liste de Notices SolR
     */
    public List<NoticeSolrV1> findNoticesByCriteria(List<Criterion> criteria, Sort sort, Pageable page) {

        SimpleQuery solrQuery = new SimpleQuery(builderQuery.buildQuery(criteria),page);
        solrQuery.addProjectionOnFields(
                NoticeV1Field.PPN,
                NoticeV1Field.ISSN_T,
                NoticeV1Field.PCP_T,
                NoticeV1Field.RCR_T,
                NoticeV1Field.EDITOR,
                NoticeV1Field.PROCESSING_GLOBAL_DATA,
                NoticeV1Field.KEY_TITLE,
                NoticeV1Field.ISSN,
                NoticeV1Field.KEY_SHORTED_TITLE,
                NoticeV1Field.PROPER_TITLE,
                NoticeV1Field.TITLE_FROM_DIFFERENT_AUTHOR,
                NoticeV1Field.PARALLEL_TITLE,
                NoticeV1Field.TITLE_COMPLEMENT,
                NoticeV1Field.SECTION_TITLE,
                NoticeV1Field.KEY_TITLE_QUALIFIER,
                NoticeV1Field.CONTINIOUS_TYPE,
                NoticeV1Field.EXTERNAL_URLS_S,
                NoticeV1Field.NB_LOC);
        solrQuery.addSort(sort);
        // Debug query
        DefaultQueryParser dqp = new DefaultQueryParser(null);
        String actualQuery = dqp.getQueryString(solrQuery, null);
        log.debug("SolR query : "+actualQuery);

        Page results = solrTemplate.queryForPage("notice",solrQuery, NoticeSolrV1.class);
        return results.getContent();
    }

    public List<NoticeSolrV1> findNoticesBySolrQuery(String query, Sort sort, Pageable page) {

        SimpleQuery solrQuery = new SimpleQuery(query,page);
        solrQuery.addProjectionOnFields(
                NoticeV1Field.PPN,
                NoticeV1Field.ISSN_T,
                NoticeV1Field.PCP_T,
                NoticeV1Field.RCR_T,
                NoticeV1Field.EDITOR,
                NoticeV1Field.PROCESSING_GLOBAL_DATA,
                NoticeV1Field.KEY_TITLE,
                NoticeV1Field.ISSN,
                NoticeV1Field.KEY_SHORTED_TITLE,
                NoticeV1Field.PROPER_TITLE,
                NoticeV1Field.TITLE_FROM_DIFFERENT_AUTHOR,
                NoticeV1Field.PARALLEL_TITLE,
                NoticeV1Field.TITLE_COMPLEMENT,
                NoticeV1Field.SECTION_TITLE,
                NoticeV1Field.KEY_TITLE_QUALIFIER,
                NoticeV1Field.CONTINIOUS_TYPE);
        solrQuery.addSort(sort);
        // Debug query
        DefaultQueryParser dqp = new DefaultQueryParser(null);
        String actualQuery = dqp.getQueryString(solrQuery, null);
        log.debug("SolR query : "+actualQuery);

        Page results = solrTemplate.queryForPage("notice",solrQuery, NoticeSolrV1.class);
        return results.getContent();
    }
}
