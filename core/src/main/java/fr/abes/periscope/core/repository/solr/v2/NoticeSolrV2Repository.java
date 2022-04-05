package fr.abes.periscope.core.repository.solr.v2;

import fr.abes.periscope.core.entity.solr.v2.NoticeV2Solr;
import org.springframework.data.solr.repository.SolrCrudRepository;

public interface NoticeSolrV2Repository extends SolrCrudRepository<NoticeV2Solr, String>, AdvancedNoticeSolrV2Repository {

}
