package fr.abes.periscope.core.repository.solr.v1;

import fr.abes.periscope.core.entity.NoticeSolr;
import org.springframework.data.solr.repository.SolrCrudRepository;

public interface NoticeV1Repository extends SolrCrudRepository<NoticeSolr, String>, AdvancedNoticeRepository {

}
