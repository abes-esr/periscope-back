package fr.abes.periscope.core.repository.solr.v2;

import fr.abes.periscope.core.entity.v2.solr.NoticeV2Solr;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.solr.repository.SolrCrudRepository;

@Qualifier("AdvancedNoticeSolrV2Repository")
public interface NoticeSolrV2Repository extends SolrCrudRepository<NoticeV2Solr, String>, AdvancedNoticeSolrV2Repository {

}
