package fr.abes.periscope.core.repository.solr.v1;

import fr.abes.periscope.core.criterion.Criterion;
import fr.abes.periscope.core.entity.v1.solr.NoticeV1Solr;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;

import java.util.List;

/**
 * @Deprecated Interface d'un dépôt de Notice SolR avec des requêtes complexes
 */
@Deprecated
public interface AdvancedNoticeSolrV1Repository {
    List<NoticeV1Solr> findNoticesByCriteria(List<Criterion> criteria, Sort sort, Pageable page);
}

