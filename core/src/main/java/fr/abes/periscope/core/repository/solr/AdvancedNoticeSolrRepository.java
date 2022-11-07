package fr.abes.periscope.core.repository.solr;

import fr.abes.periscope.core.criterion.Criterion;
import fr.abes.periscope.core.criterion.CriterionFacette;
import fr.abes.periscope.core.entity.solr.NoticeSolr;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.solr.core.query.result.FacetPage;

import java.util.List;

/**
 * Interface d'un dépôt de Notice SolR avec des requêtes complexes
 */
public interface AdvancedNoticeSolrRepository {
    List<NoticeSolr> findNoticesByCriteria(List<Criterion> criteria, Sort sort, Pageable page);

    FacetPage<NoticeSolr> findNoticesWithFacetQuery(List<Criterion> criteriaNotice, List<Criterion> criteriaExemp, List<String> facettes, List<CriterionFacette> facetteFilter, Sort sort, Pageable page);
}

