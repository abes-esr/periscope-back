package fr.abes.periscope.core.repository;

import fr.abes.periscope.core.entity.NoticeSolr;
import fr.abes.periscope.core.util.TrackExecutionTime;
import org.springframework.data.domain.Pageable;
import org.springframework.data.solr.repository.Query;
import org.springframework.data.solr.repository.SolrCrudRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface NoticeRepository extends SolrCrudRepository<NoticeSolr, String> {

    @TrackExecutionTime
    @Query(value="001_s:?0",
            fields={"001_s",
                    "011-a_t",
                    "930-z_t",
                    "930-b_t",
                    "210-c_z",
                    "100-a_t",
                    "210-c_z",
                    "530-a_z",
                    "011-a_z",
                    "531-a_z",
                    "200-a_z",
                    "200-c_z",
                    "200-d_z",
                    "200-e_z",
                    "200-i_z",
                    "530-b_z",
                    "110-a_z"}
    )
    NoticeSolr findByPpn(String ppn);

    @TrackExecutionTime
    @Query(value="930-z_s:?0",
            fields={"001_s",
                    "011-a_t",
                    "930-z_t",
                    "930-b_t",
                    "210-c_z",
                    "100-a_t",
                    "210-c_z",
                    "530-a_z",
                    "011-a_z",
                    "531-a_z",
                    "200-a_z",
                    "200-c_z",
                    "200-d_z",
                    "200-e_z",
                    "200-i_z",
                    "530-b_z",
                    "110-a_z"}
    )
    List<NoticeSolr> findNoticesByPcp(String pcp, Pageable pageable);

    @TrackExecutionTime
    //q=((930-z_s:PCAq%20OR%20930-z_s:PCLR%20OR%20930-z_s:PCLim)%20OR%20(930-b_t:341722106%20OR%20930-b_t:341722203))%20AND%20(101-a_t:fre%20AND%20530-a_t:[*%20TO%20*])
    @Query(value="((930-z_s:?0 OR 930-z_s:PCLR OR 930-z_s:PCLim) OR (930-b_t:341722106 OR 930-b_t:341722203)) AND (101-a_t:fre AND 530-a_t:[* TO *])",
            fields={"001_s",
                    "011-a_t",
                    "930-z_t",
                    "930-b_t",
                    "210-c_z",
                    "100-a_t",
                    "210-c_z",
                    "530-a_z",
                    "011-a_z",
                    "531-a_z",
                    "200-a_z",
                    "200-c_z",
                    "200-d_z",
                    "200-e_z",
                    "200-i_z",
                    "530-b_z",
                    "110-a_z"}
    )
    List<NoticeSolr> findNoticesByPcpComplex(String pcp, Pageable pageable);
}
