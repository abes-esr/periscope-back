package fr.abes.periscope.core.repository.solr.impl;

import fr.abes.periscope.core.criterion.Criterion;
import fr.abes.periscope.core.criterion.CriterionFacette;
import fr.abes.periscope.core.entity.solr.ItemSolrField;
import fr.abes.periscope.core.entity.solr.NoticeSolrField;
import fr.abes.periscope.core.entity.solr.NoticeSolr;
import fr.abes.periscope.core.repository.solr.AdvancedNoticeSolrRepository;
import fr.abes.periscope.core.repository.solr.SolrQueryBuilder;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
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
public class AdvancedNoticeSolrRepositoryImpl implements AdvancedNoticeSolrRepository {

    private final SolrTemplate solrTemplate;

    private final SolrQueryBuilder builderQuery;

    @Value("${periscope.solr.v2.core}")
    private String core;

    @Value("${periscope.solr.v2.nbexemplaires}")
    private String nbExemplaires;

    public AdvancedNoticeSolrRepositoryImpl(SolrTemplate template, SolrQueryBuilder builder) {
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
    public List<NoticeSolr> findNoticesByCriteria(List<Criterion> criteria, Sort sort, Pageable page) {

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
     * @return List<NoticeSolr> Liste de Notices SolR
     */
    public List<NoticeSolr> findNoticesBySolrQuery(String query, Sort sort, Pageable page) {

        SimpleQuery solrQuery = new SimpleQuery(query, page);
        setProjectionsFieldsOnQuery(solrQuery);
        solrQuery.addSort(sort);
        solrQuery.addProjectionOnField("[child]");
        // Debug query
        DefaultQueryParser dqp = new DefaultQueryParser(null);
        String actualQuery = dqp.getQueryString(solrQuery, null);
        log.debug("SolR query : " + actualQuery);

        Page results = solrTemplate.queryForPage(core, solrQuery, NoticeSolr.class);
        return results.getContent();
    }

    @Override
    public FacetPage<NoticeSolr> findNoticesWithFacetQuery(List<Criterion> criteriaNotice, List<Criterion> criteriaExemp, List<String> facettes, List<CriterionFacette> facetteFilter, Sort sort, Pageable page) {
        FacetQuery query = builderQuery.constructFacetQuery(criteriaNotice, criteriaExemp);
        query = builderQuery.addFacetsFilters(query, facetteFilter);

        //request handler nécessaire pour les facettes au niveau exemplaire
        query.setRequestHandler("bjqfacet");
        setProjectionsFieldsOnQuery(query);

        query.addSort(sort);
        query.setPageRequest(page);

        query = builderQuery.addFacetsNotices(query, facettes);
        //query = builderQuery.addFacetsExemplaires(query, facettes);
        return solrTemplate.queryForFacetPage(core, query, NoticeSolr.class);
    }

    private void setProjectionsFieldsOnQuery(Query query) {
        query.addProjectionOnField(new SimpleField(NoticeSolrField.ID));
        query.addProjectionOnField(new SimpleField(NoticeSolrField.PPN));
        query.addProjectionOnField(new SimpleField(NoticeSolrField.EDITOR));
        query.addProjectionOnField(new SimpleField(NoticeSolrField.EDITOR_Z));
        query.addProjectionOnField(new SimpleField(NoticeSolrField.PROCESSING_GLOBAL_DATA));
        query.addProjectionOnField(new SimpleField(NoticeSolrField.KEY_TITLE));
        query.addProjectionOnField(new SimpleField(NoticeSolrField.ISSN));
        query.addProjectionOnField(new SimpleField(NoticeSolrField.KEY_SHORTED_TITLE));
        query.addProjectionOnField(new SimpleField(NoticeSolrField.KEY_SHORTED_TITLE_Z));
        query.addProjectionOnField(new SimpleField(NoticeSolrField.PROPER_TITLE));
        query.addProjectionOnField(new SimpleField(NoticeSolrField.PROPER_TITLE_Z));
        query.addProjectionOnField(new SimpleField(NoticeSolrField.TITLE_FROM_DIFFERENT_AUTHOR));
        query.addProjectionOnField(new SimpleField(NoticeSolrField.TITLE_FROM_DIFFERENT_AUTHOR_Z));
        query.addProjectionOnField(new SimpleField(NoticeSolrField.PARALLEL_TITLE));
        query.addProjectionOnField(new SimpleField(NoticeSolrField.PARALLEL_TITLE_Z));
        query.addProjectionOnField(new SimpleField(NoticeSolrField.TITLE_COMPLEMENT));
        query.addProjectionOnField(new SimpleField(NoticeSolrField.TITLE_COMPLEMENT_Z));
        query.addProjectionOnField(new SimpleField(NoticeSolrField.SECTION_TITLE));
        query.addProjectionOnField(new SimpleField(NoticeSolrField.SECTION_TITLE_Z));
        query.addProjectionOnField(new SimpleField(NoticeSolrField.KEY_TITLE_QUALIFIER));
        query.addProjectionOnField(new SimpleField(NoticeSolrField.DOCUMENT_TYPE));
        query.addProjectionOnField(new SimpleField(NoticeSolrField.SUPPORT_TYPE));
        query.addProjectionOnField(new SimpleField(NoticeSolrField.EXTERNAL_URLS));
        query.addProjectionOnField(new SimpleField(NoticeSolrField.NB_LOC));
        query.addProjectionOnField(new SimpleField(NoticeSolrField.NB_PCP));
        query.addProjectionOnField(new SimpleField(NoticeSolrField.LANGUAGE));
        query.addProjectionOnField(new SimpleField(NoticeSolrField.COUNTRY));
        query.addProjectionOnField(new SimpleField(NoticeSolrField.START_YEAR));
        query.addProjectionOnField(new SimpleField(NoticeSolrField.START_YEAR_CONFIDENCE_INDEX));
        query.addProjectionOnField(new SimpleField(NoticeSolrField.END_YEAR));
        query.addProjectionOnField(new SimpleField(NoticeSolrField.END_YEAR_CONFIDENCE_INDEX));
        query.addProjectionOnField(new SimpleField(NoticeSolrField.RCR_LIST));
        query.addProjectionOnField(new SimpleField(NoticeSolrField.PCP_LIST));
        query.addProjectionOnField(new SimpleField(ItemSolrField.EPN));
        query.addProjectionOnField(new SimpleField(ItemSolrField.PPN_PARENT));
        query.addProjectionOnField(new SimpleField(ItemSolrField.RCR));
        query.addProjectionOnField(new SimpleField(ItemSolrField.PCP));

        query.addProjectionOnField(new SimpleField("[child limit=" + nbExemplaires + "]"));
    }


}
