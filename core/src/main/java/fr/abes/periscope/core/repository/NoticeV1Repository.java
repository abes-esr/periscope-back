package fr.abes.periscope.core.repository;

import fr.abes.periscope.core.entity.NoticeSolrV1;
import fr.abes.periscope.core.repository.solr.AdvancedNoticeRepository;
import org.springframework.data.solr.repository.SolrCrudRepository;

public interface NoticeV1Repository extends SolrCrudRepository<NoticeSolrV1, String>, AdvancedNoticeRepository {

}
