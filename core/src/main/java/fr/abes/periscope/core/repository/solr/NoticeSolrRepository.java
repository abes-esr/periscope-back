package fr.abes.periscope.core.repository.solr;

import fr.abes.periscope.core.entity.solr.NoticeSolr;
import org.springframework.data.solr.repository.SolrCrudRepository;

public interface NoticeSolrRepository extends SolrCrudRepository<NoticeSolr, String>, AdvancedNoticeSolrRepository {
    NoticeSolr findByPpn(String ppn);
}
