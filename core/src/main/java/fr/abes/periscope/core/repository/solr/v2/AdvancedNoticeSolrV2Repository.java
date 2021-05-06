package fr.abes.periscope.core.repository.solr.v2;

import fr.abes.periscope.core.criterion.Criterion;
import fr.abes.periscope.core.entity.v1.solr.NoticeV1Solr;
import fr.abes.periscope.core.entity.v2.solr.NoticeV2Solr;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;

import java.util.List;

/**
 * Interface d'un dépôt de Notice SolR avec des requêtes complexes
 */
public interface AdvancedNoticeSolrV2Repository {
    List<NoticeV2Solr> findNoticesByCriteria(List<Criterion> criteria, Sort sort, Pageable page);
}

