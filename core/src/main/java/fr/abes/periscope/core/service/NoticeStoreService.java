package fr.abes.periscope.core.service;

import fr.abes.periscope.core.criterion.Criterion;
import fr.abes.periscope.core.criterion.CriterionSort;
import fr.abes.periscope.core.entity.Notice;
import fr.abes.periscope.core.entity.NoticeSolr;
import fr.abes.periscope.core.repository.NoticeRepository;
import fr.abes.periscope.core.repository.solr.NoticeField;
import fr.abes.periscope.core.util.NoticeMapper;
import fr.abes.periscope.core.util.TrackExecutionTime;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.stream.Stream;

/**
 * Représente la couche service pour les Notices
 */
@Slf4j
@Service
public class NoticeStoreService {

    private final NoticeRepository noticeRepository;

    private final NoticeMapper noticeMapper;

    @Autowired
    public NoticeStoreService(NoticeRepository noticeRepository, NoticeMapper mapper) {
        this.noticeRepository = noticeRepository;
        this.noticeMapper = mapper;
    }

    /**
     * Retourne une liste de Notice en fonction des critères de recherche et du numéro de page
     * @param criteria Critères de recherche
     * @param page Numéro de page
     * @param size Nombre d'élément
     * @return List<Notice> Liste de Notice répondant aux critères de recherche
     */
    @TrackExecutionTime
    public List<Notice> findNoticesByCriteria(List<Criterion> criteria, List<CriterionSort> criteriaSort, int page, int size) {
        List<Sort.Order> orders = new ArrayList<Sort.Order>();
        if (criteriaSort.size() == 0) {
            orders.add(new Sort.Order(Sort.Direction.ASC, NoticeField.PPN));
        }
        else {
            criteriaSort.forEach(c -> {
                orders.add(new Sort.Order(c.getOrder(), c.getSort()));
            });
        }
        List<NoticeSolr> notices = noticeRepository.findNoticesByCriteria(criteria, Sort.by(orders), PageRequest.of(page, size));
        return noticeMapper.mapList(notices);
    }
}
