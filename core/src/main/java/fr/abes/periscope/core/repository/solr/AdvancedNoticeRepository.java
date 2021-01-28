package fr.abes.periscope.core.repository.solr;

import fr.abes.periscope.core.criterion.Criterion;
import fr.abes.periscope.core.entity.NoticeSolr;
import org.springframework.data.domain.Pageable;

import java.util.List;

/**
 * Interface d'un dépôt de Notice SolR avec des requêtes complexes
 */
public interface AdvancedNoticeRepository {

    List<NoticeSolr> findNoticesByCriteria(List<Criterion> criteria, Pageable page);
}
