package fr.abes.periscope.core.repository;

import fr.abes.periscope.core.entity.NoticeSolr;
import fr.abes.periscope.core.repository.solr.AdvancedNoticeRepository;
import org.springframework.data.solr.repository.SolrCrudRepository;

public interface NoticeRepository extends SolrCrudRepository<NoticeSolr, String>, AdvancedNoticeRepository {

}
