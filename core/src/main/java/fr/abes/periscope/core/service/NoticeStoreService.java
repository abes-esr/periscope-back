package fr.abes.periscope.core.service;

import fr.abes.periscope.core.criterion.Criterion;
import fr.abes.periscope.core.criterion.CriterionSort;
import fr.abes.periscope.core.entity.Notice;
import fr.abes.periscope.core.entity.NoticeSolr;
import fr.abes.periscope.core.repository.solr.v1.NoticeV1Repository;
import fr.abes.periscope.core.repository.solr.v2.NoticeV2Repository;
import fr.abes.periscope.core.util.NoticeMapper;
import fr.abes.periscope.core.util.TrackExecutionTime;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Bean;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.data.solr.core.SolrTemplate;
import org.springframework.stereotype.Service;

import java.util.*;

/**
 * Représente la couche service pour les Notices
 */
@Slf4j
@Service
@Data
public class NoticeStoreService {

    @Autowired
    @Qualifier("solr-v1")
    private SolrTemplate solrV1Template;

    @Autowired
    @Qualifier("solr-v2")
    private SolrTemplate solrV2Template;

    private final NoticeV1Repository noticeV1Repository;
    private final NoticeV2Repository noticeV2Repository;

    @Bean
    public ModelMapper modelMapper() {
        return new ModelMapper();
    }

    private final NoticeMapper noticeMapper;

    /**
     * Retourne une liste de Notice en fonction des critères de recherche et du numéro de page
     *
     * @param criteria Critères de recherche
     * @param page     Numéro de page
     * @param size     Nombre d'élément
     * @return List<Notice> Liste de Notice répondant aux critères de recherche
     */
    @TrackExecutionTime
    public List<Notice> findNoticesByCriteria(List<Criterion> criteria, List<CriterionSort> criteriaSort, int page, int size) {
        List<Sort.Order> orders = new ArrayList<>();
        criteriaSort.forEach(c -> {
            orders.add(new Sort.Order(c.getOrder(), c.getSort()));
        });

        List<NoticeSolr> notices = noticeV1Repository.findNoticesByCriteria(criteria, Sort.by(orders), PageRequest.of(page, size));
        return noticeMapper.mapList(notices,Notice.class);
    }
}
