package fr.abes.periscope.core.repository.solr.v1;

import fr.abes.periscope.core.entity.solr.v1.NoticeV1Solr;
import org.springframework.data.solr.repository.SolrCrudRepository;

@Deprecated
public interface NoticeSolrV1Repository extends SolrCrudRepository<NoticeV1Solr, String>, AdvancedNoticeSolrV1Repository {

}
