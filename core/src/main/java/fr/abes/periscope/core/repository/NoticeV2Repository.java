package fr.abes.periscope.core.repository;

import fr.abes.periscope.core.entity.NoticeSolrV2;
import fr.abes.periscope.core.repository.solr.AdvancedNoticeRepository;
import org.springframework.data.solr.repository.SolrCrudRepository;

public interface NoticeV2Repository extends SolrCrudRepository<NoticeSolrV2, String>, AdvancedNoticeRepository {
}
