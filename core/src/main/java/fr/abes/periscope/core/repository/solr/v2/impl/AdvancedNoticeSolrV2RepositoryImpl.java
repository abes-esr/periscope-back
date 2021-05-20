package fr.abes.periscope.core.repository.solr.v2.impl;

import fr.abes.periscope.core.criterion.Criterion;
import fr.abes.periscope.core.entity.v2.solr.ItemSolrField;
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
import org.springframework.data.solr.core.query.*;
import org.springframework.data.solr.core.query.result.FacetPage;
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
     *
     * @param criteria Les critères de la recherche
     * @param sort     Les critères de tri
     * @param page     La page souhaitée
     * @return List<NoticeV2Solr> Liste de Notices SolR
     */
    @Override
    public List<NoticeV2Solr> findNoticesByCriteria(List<Criterion> criteria, Sort sort, Pageable page) {

        SimpleQuery solrQuery = new SimpleQuery(builderQuery.buildQuery(criteria), page);

        DefaultQueryParser dqp = new DefaultQueryParser(null);
        String actualQuery = dqp.getQueryString(solrQuery, null);

        return findNoticesBySolrQuery(actualQuery, sort, page);
    }

    /**
     * Retourne les Notices SolR selon une requête SolR, un critère de tri
     * et une page.
     * La liste des champs SolR a récupéré est inscrites en dur dans la méthode
     *
     * @param query Requête SolR
     * @param sort  Les critères de tri
     * @param page  La page souhaitée
     * @return List<NoticeV2Solr> Liste de Notices SolR
     */
    public List<NoticeV2Solr> findNoticesBySolrQuery(String query, Sort sort, Pageable page) {

        SimpleQuery solrQuery = new SimpleQuery("{!parent which=notice_type:notice}" + query, page);
        setProjectionsFieldsOnQuery(solrQuery);
        solrQuery.addSort(sort);
        solrQuery.addProjectionOnField("[child]");
        // Debug query
        DefaultQueryParser dqp = new DefaultQueryParser(null);
        String actualQuery = dqp.getQueryString(solrQuery, null);
        log.debug("SolR query : " + actualQuery);

        Page results = solrTemplate.queryForPage(core, solrQuery, NoticeV2Solr.class);
        return results.getContent();
    }

    @Override
    public FacetPage<NoticeV2Solr> findNoticesWithFacetQuery(List<Criterion> criteriaNotice, List<Criterion> criteriaExemp, List<String> facettes, Sort sort, Pageable page) {
        FacetQuery query = builderQuery.constructFacetQuery(criteriaNotice, criteriaExemp, page);

        //request handler nécessaire pour les facettes au niveau exemplaire
        query.setRequestHandler("bjqfacet");
        setProjectionsFieldsOnQuery(query);

        query.addSort(sort);
        query = builderQuery.addFacetsNotices(query, facettes);
        return solrTemplate.queryForFacetPage(core, query, NoticeV2Solr.class);
    }

    private void setProjectionsFieldsOnQuery(Query query) {
        query.addProjectionOnField(new SimpleField(NoticeV2SolrField.ID));
        query.addProjectionOnField(new SimpleField(NoticeV2SolrField.PPN));
        query.addProjectionOnField(new SimpleField(NoticeV2SolrField.EDITOR));
        query.addProjectionOnField(new SimpleField(NoticeV2SolrField.EDITOR_Z));
        query.addProjectionOnField(new SimpleField(NoticeV2SolrField.PROCESSING_GLOBAL_DATA));
        query.addProjectionOnField(new SimpleField(NoticeV2SolrField.KEY_TITLE));
        query.addProjectionOnField(new SimpleField(NoticeV2SolrField.ISSN));
        query.addProjectionOnField(new SimpleField(NoticeV2SolrField.KEY_SHORTED_TITLE));
        query.addProjectionOnField(new SimpleField(NoticeV2SolrField.KEY_SHORTED_TITLE_Z));
        query.addProjectionOnField(new SimpleField(NoticeV2SolrField.PROPER_TITLE));
        query.addProjectionOnField(new SimpleField(NoticeV2SolrField.PROPER_TITLE_Z));
        query.addProjectionOnField(new SimpleField(NoticeV2SolrField.TITLE_FROM_DIFFERENT_AUTHOR));
        query.addProjectionOnField(new SimpleField(NoticeV2SolrField.TITLE_FROM_DIFFERENT_AUTHOR_Z));
        query.addProjectionOnField(new SimpleField(NoticeV2SolrField.PARALLEL_TITLE));
        query.addProjectionOnField(new SimpleField(NoticeV2SolrField.PARALLEL_TITLE_Z));
        query.addProjectionOnField(new SimpleField(NoticeV2SolrField.TITLE_COMPLEMENT));
        query.addProjectionOnField(new SimpleField(NoticeV2SolrField.TITLE_COMPLEMENT_Z));
        query.addProjectionOnField(new SimpleField(NoticeV2SolrField.SECTION_TITLE));
        query.addProjectionOnField(new SimpleField(NoticeV2SolrField.SECTION_TITLE_Z));
        query.addProjectionOnField(new SimpleField(NoticeV2SolrField.KEY_TITLE_QUALIFIER));
        query.addProjectionOnField(new SimpleField(NoticeV2SolrField.DOCUMENT_TYPE));
        query.addProjectionOnField(new SimpleField(NoticeV2SolrField.SUPPORT_TYPE));
        query.addProjectionOnField(new SimpleField(NoticeV2SolrField.EXTERNAL_URLS));
        query.addProjectionOnField(new SimpleField(NoticeV2SolrField.NB_LOC));
        query.addProjectionOnField(new SimpleField(NoticeV2SolrField.LANGUAGE));
        query.addProjectionOnField(new SimpleField(NoticeV2SolrField.COUNTRY));
        query.addProjectionOnField(new SimpleField(NoticeV2SolrField.START_YEAR));
        query.addProjectionOnField(new SimpleField(NoticeV2SolrField.START_YEAR_CONFIDENCE_INDEX));
        query.addProjectionOnField(new SimpleField(NoticeV2SolrField.END_YEAR));
        query.addProjectionOnField(new SimpleField(NoticeV2SolrField.END_YEAR_CONFIDENCE_INDEX));
        query.addProjectionOnField(new SimpleField(ItemSolrField.EPN));
        query.addProjectionOnField(new SimpleField(ItemSolrField.PPN_PARENT));
        query.addProjectionOnField(new SimpleField(ItemSolrField.RCR));
        query.addProjectionOnField(new SimpleField(ItemSolrField.PCP));

        query.addProjectionOnField(new SimpleField("[child limit=100]"));
    }


}
