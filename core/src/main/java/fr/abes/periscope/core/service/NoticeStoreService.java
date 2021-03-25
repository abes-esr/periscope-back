package fr.abes.periscope.core.service;

import fr.abes.periscope.core.criterion.Criterion;
import fr.abes.periscope.core.criterion.CriterionSort;
import fr.abes.periscope.core.entity.Notice;
import fr.abes.periscope.core.entity.v1.solr.NoticeV1Solr;
import fr.abes.periscope.core.entity.v2.solr.NoticeV2Solr;
import fr.abes.periscope.core.repository.solr.v1.NoticeSolrV1Repository;
import fr.abes.periscope.core.repository.solr.v2.NoticeSolrV2Repository;
import fr.abes.periscope.core.util.NoticeMapper;
import fr.abes.periscope.core.util.TrackExecutionTime;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;

/**
 * Représente la couche service pour les Notices
 */
@Slf4j
@Service
@Data
public class NoticeStoreService {

    private NoticeSolrV1Repository noticeV1Repository;
    private NoticeSolrV2Repository noticeV2Repository;
    private NoticeMapper noticeMapper;

    private final String defaultRepository = "v1";

    @Autowired
    public NoticeStoreService(NoticeMapper mapper, NoticeSolrV1Repository noticeV1Repository, NoticeSolrV2Repository noticeV2Repository) {
        this.noticeV1Repository = noticeV1Repository;
        this.noticeV2Repository = noticeV2Repository;
        this.noticeMapper = mapper;
    }

    /**
     * Retourne une liste de Notice en fonction des critères de recherche et du numéro de page
     *
     * @param repository Repository à utiliser
     * @param criteria Critères de recherche
     * @param page     Numéro de page
     * @param size     Nombre d'élément
     * @return List<Notice> Liste de Notice répondant aux critères de recherche
     * @throws IllegalArgumentException Si le repository ne peut pas être décodé
     */
    @TrackExecutionTime
    public List<Notice> findNoticesByCriteria(String repository, List<Criterion> criteria, List<CriterionSort> criteriaSort, int page, int size) throws IllegalArgumentException {
        List<Sort.Order> orders = new ArrayList<>();
        criteriaSort.forEach(c -> {
            orders.add(new Sort.Order(c.getOrder(), c.getSort()));
        });

        switch (repository) {
            case "v1":
                List<NoticeV1Solr> noticesV1 = noticeV1Repository.findNoticesByCriteria(criteria, Sort.by(orders), PageRequest.of(page, size));
                return noticeMapper.mapList(noticesV1,Notice.class);
            case "v2":
                List<NoticeV2Solr> noticesV2 = noticeV2Repository.findNoticesByCriteria(criteria, Sort.by(orders), PageRequest.of(page, size));
                return noticeMapper.mapList(noticesV2,Notice.class);
            default:
                throw new IllegalArgumentException("Unable to decode repository :"+repository);
        }
    }

    /**
     * Retourne une liste de Notice en fonction des critères de recherche et du numéro de page
     * Le repository par défaut est utilisé
     *
     * @param criteria Critères de recherche
     * @param page     Numéro de page
     * @param size     Nombre d'élément
     * @return List<Notice> Liste de Notice répondant aux critères de recherche
     */
    @TrackExecutionTime
    public List<Notice> findNoticesByCriteria(List<Criterion> criteria, List<CriterionSort> criteriaSort, int page, int size) {
        return findNoticesByCriteria(defaultRepository,criteria,criteriaSort,page,size);
    }
}
