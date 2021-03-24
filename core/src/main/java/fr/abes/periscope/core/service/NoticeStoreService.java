package fr.abes.periscope.core.service;

import fr.abes.periscope.core.criterion.Criterion;
import fr.abes.periscope.core.criterion.CriterionSort;
import fr.abes.periscope.core.entity.Notice;
import fr.abes.periscope.core.entity.NoticeSolrV1;
import fr.abes.periscope.core.repository.NoticeV1Repository;
import fr.abes.periscope.core.util.NoticeMapper;
import fr.abes.periscope.core.util.TrackExecutionTime;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import java.util.*;

/**
 * Représente la couche service pour les Notices
 */
@Slf4j
@Service
@Data
public class NoticeStoreService {

    private final NoticeV1Repository noticeRepository;

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
        List<NoticeSolrV1> notices = noticeRepository.findNoticesByCriteria(criteria, Sort.by(orders), PageRequest.of(page, size));
        return noticeMapper.mapList(notices);
    }
}
