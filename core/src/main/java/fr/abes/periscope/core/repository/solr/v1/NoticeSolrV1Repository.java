package fr.abes.periscope.core.repository.solr.v1;

import fr.abes.periscope.core.entity.v1.solr.NoticeV1Solr;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.solr.repository.SolrCrudRepository;

@Qualifier("AdvancedNoticeSolrV1Repository")
public interface NoticeSolrV1Repository extends SolrCrudRepository<NoticeV1Solr, String>, AdvancedNoticeSolrV1Repository {

}
