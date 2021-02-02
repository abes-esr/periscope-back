package fr.abes.periscope.core.repository.solr.impl;

import fr.abes.periscope.core.criterion.Criterion;
import fr.abes.periscope.core.entity.NoticeSolr;
import fr.abes.periscope.core.repository.solr.AdvancedNoticeRepository;
import fr.abes.periscope.core.repository.solr.NoticeField;
import fr.abes.periscope.core.repository.solr.SolrQueryBuilder;
import fr.abes.periscope.core.util.TrackExecutionTime;
import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.solr.core.DefaultQueryParser;
import org.springframework.data.solr.core.SolrTemplate;
import org.springframework.data.solr.core.query.SimpleQuery;

import java.util.List;

/**
 * Représente un dépôt de Notice SolR avec des requêtes complexes
 */
@AllArgsConstructor
@Slf4j
public class AdvancedNoticeRepositoryImpl implements AdvancedNoticeRepository {

    private SolrTemplate solrTemplate;

    private SolrQueryBuilder builderQuery;

    @TrackExecutionTime
    @Override
    /**
     * Retourne les Notices SolR selon une liste de critères et une page
     * @param criteria Les critères de la recherche
     * @param page La page souhaitée
     * @return List<NoticeSolr> Liste de Notices SolR
     */
    public List<NoticeSolr> findNoticesByCriteria(List<Criterion> criteria, Pageable page) {

        SimpleQuery solrQuery = new SimpleQuery(builderQuery.buildQuery(criteria),page);
        solrQuery.addProjectionOnFields(
                NoticeField.PPN,
                NoticeField.ISSN_T,
                NoticeField.PCP_T,
                NoticeField.RCR_T,
                NoticeField.EDITOR,
                NoticeField.PROCESSING_GLOBAL_DATA,
                NoticeField.KEY_TITLE,
                NoticeField.ISSN,
                NoticeField.KEY_SHORTED_TITLE,
                NoticeField.PROPER_TITLE,
                NoticeField.TITLE_FROM_DIFFERENT_AUTHOR,
                NoticeField.PARALLEL_TITLE,
                NoticeField.TITLE_COMPLEMENT,
                NoticeField.SECTION_TITLE,
                NoticeField.KEY_TITLE_QUALIFIER,
                NoticeField.CONTINIOUS_TYPE);

        // Debug query
        DefaultQueryParser dqp = new DefaultQueryParser(null);
        String actualQuery = dqp.getQueryString(solrQuery, null);
        log.debug("SolR query : "+actualQuery);

        Page results = solrTemplate.queryForPage("notice",solrQuery, NoticeSolr.class);
        return results.getContent();
    }

    public List<NoticeSolr> findNoticesBySolrQuery(String query, Pageable page) {

        SimpleQuery solrQuery = new SimpleQuery(query,page);
        solrQuery.addProjectionOnFields(
                NoticeField.PPN,
                NoticeField.ISSN_T,
                NoticeField.PCP_T,
                NoticeField.RCR_T,
                NoticeField.EDITOR,
                NoticeField.PROCESSING_GLOBAL_DATA,
                NoticeField.KEY_TITLE,
                NoticeField.ISSN,
                NoticeField.KEY_SHORTED_TITLE,
                NoticeField.PROPER_TITLE,
                NoticeField.TITLE_FROM_DIFFERENT_AUTHOR,
                NoticeField.PARALLEL_TITLE,
                NoticeField.TITLE_COMPLEMENT,
                NoticeField.SECTION_TITLE,
                NoticeField.KEY_TITLE_QUALIFIER,
                NoticeField.CONTINIOUS_TYPE);

        // Debug query
        DefaultQueryParser dqp = new DefaultQueryParser(null);
        String actualQuery = dqp.getQueryString(solrQuery, null);
        log.debug("SolR query : "+actualQuery);

        Page results = solrTemplate.queryForPage("notice",solrQuery, NoticeSolr.class);
        return results.getContent();
    }
}
