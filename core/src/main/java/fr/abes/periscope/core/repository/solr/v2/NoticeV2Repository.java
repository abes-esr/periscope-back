package fr.abes.periscope.core.repository.solr.v2;

import fr.abes.periscope.core.entity.v2.solr.NoticeV2Solr;
import org.springframework.data.solr.repository.SolrCrudRepository;

public interface NoticeV2Repository extends SolrCrudRepository<NoticeV2Solr, String>, AdvancedNoticeRepository {

}
