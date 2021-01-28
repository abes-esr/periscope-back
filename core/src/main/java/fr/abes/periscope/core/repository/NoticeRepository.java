package fr.abes.periscope.core.repository;

import fr.abes.periscope.core.entity.Notice;
import fr.abes.periscope.core.entity.NoticeSolr;
import fr.abes.periscope.core.repository.solr.AdvancedNoticeRepository;
import fr.abes.periscope.core.repository.solr.NoticeField;
import org.springframework.data.domain.Pageable;
import org.springframework.data.solr.repository.Query;
import org.springframework.data.solr.repository.SolrCrudRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface NoticeRepository extends SolrCrudRepository<NoticeSolr, String>, AdvancedNoticeRepository {

}
