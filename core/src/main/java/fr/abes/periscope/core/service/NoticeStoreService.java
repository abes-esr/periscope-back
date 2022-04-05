package fr.abes.periscope.core.service;

import fr.abes.periscope.core.criterion.Criterion;
import fr.abes.periscope.core.criterion.CriterionFacette;
import fr.abes.periscope.core.criterion.CriterionSort;
import fr.abes.periscope.core.entity.solr.Notice;
import fr.abes.periscope.core.entity.solr.v1.NoticeV1Solr;
import fr.abes.periscope.core.entity.solr.v2.FacetteSolr;
import fr.abes.periscope.core.entity.solr.v2.NoticeV2Solr;
import fr.abes.periscope.core.entity.solr.v2.ResultSolr;
import fr.abes.periscope.core.repository.solr.v1.NoticeSolrV1Repository;
import fr.abes.periscope.core.repository.solr.v2.NoticeSolrV2Repository;
import fr.abes.periscope.core.util.UtilsMapper;
import fr.abes.periscope.core.util.TYPE_NOTICE;
import fr.abes.periscope.core.util.TrackExecutionTime;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.data.solr.core.query.result.FacetFieldEntry;
import org.springframework.data.solr.core.query.result.FacetPage;
import org.springframework.stereotype.Service;

import java.util.*;

/**
 * Représente la couche service pour les Notices
 */
@Slf4j
@Service
@Data
public class NoticeStoreService {

    private NoticeSolrV1Repository noticeV1Repository;
    private NoticeSolrV2Repository noticeV2Repository;
    private UtilsMapper utilsMapper;

    private static final String DEFAULT_REPOSITORY = "v1";

    @Autowired
    public NoticeStoreService(UtilsMapper mapper, NoticeSolrV1Repository noticeV1Repository, NoticeSolrV2Repository noticeV2Repository) {
        this.noticeV1Repository = noticeV1Repository;
        this.noticeV2Repository = noticeV2Repository;
        this.utilsMapper = mapper;
    }

    /**
     * Retourne une liste de Notice en fonction des critères de recherche,
     * du critère de tri et du numéro de page     *
     *
     * @param repository   Repository à utiliser
     * @param criteria     Les critères de recherche
     * @param criteriaSort Les critères de tri
     * @param page         Numéro de page
     * @param size         Nombre d'élément
     * @return List<Notice> Liste de Notice répondant aux critères de recherche
     * @throws IllegalArgumentException Si le repository ne peut pas être décodé
     */
    @TrackExecutionTime
    public List<Notice> findNoticesByCriteria(String repository, List<Criterion> criteria, List<CriterionSort> criteriaSort, int page, int size) throws IllegalArgumentException {
        List<Sort.Order> orders = new ArrayList<>();
        criteriaSort.forEach(c -> orders.add(new Sort.Order(c.getOrder(), c.getSort())));

        switch (repository) {
            case "v1":
                List<NoticeV1Solr> noticesV1 = noticeV1Repository.findNoticesByCriteria(criteria, Sort.by(orders), PageRequest.of(page, size));
                return utilsMapper.mapList(noticesV1, Notice.class);
            case "v2":
                List<NoticeV2Solr> noticesV2 = noticeV2Repository.findNoticesByCriteria(criteria, Sort.by(orders), PageRequest.of(page, size));
                return utilsMapper.mapList(noticesV2, Notice.class);
            default:
                throw new IllegalArgumentException("Unable to decode repository :" + repository);
        }
    }

    /**
     * Retourne une liste de Notice en fonction des critères de recherche,
     * du critère de tri et du numéro de page avec le repository par défaut     *
     *
     * @param criteria     Les critères de recherche
     * @param criteriaSort Les critères de tri
     * @param page         Numéro de page
     * @param size         Nombre d'élément
     * @return List<Notice> Liste de Notice répondant aux critères de recherche
     */
    @TrackExecutionTime
    public List<Notice> findNoticesByCriteria(List<Criterion> criteria, List<CriterionSort> criteriaSort, int page, int size) {
        return findNoticesByCriteria(DEFAULT_REPOSITORY, criteria, criteriaSort, page, size);
    }

    /**
     * Retourne une liste de notices, une liste de facettes et le nombre de pages total
     * en fonction des critères de recherche, des critères de tri et du numéro de page (fonctionne uniquement sur la V2 de l'API)
     *
     * @param criteriaNotice les critères de recherche
     * @param facettes       liste des facettes (uniquement sur des zones de la notice bibliographique)
     * @param facetteFilter liste des filtres à appliquer aux facettes sélectionnées
     * @param criterionSorts les critères de tri
     * @param page           numéro de page
     * @param size           nombre d'élément par page
     * @return list de résultat comprendre la liste des notices, la liste des facettes et le nombre de page total du jeu de résultat
     */
    public ResultSolr findNoticesWithFacets(List<Criterion> criteriaNotice, List<String> facettes, List<CriterionFacette> facetteFilter, List<CriterionSort> criterionSorts, int page, int size) {
        List<Criterion> criteresBiblio = new LinkedList<>();
        List<Criterion> criteresExemp = new LinkedList<>();
        List<Sort.Order> orders = new ArrayList<>();
        criterionSorts.forEach(c -> orders.add(new Sort.Order(c.getOrder(), c.getSort())));

        criteriaNotice.forEach(c -> {
            if (c.getTypeNotice().equals(TYPE_NOTICE.BIBLIO)) {
                criteresBiblio.add(c);
            }
            if (c.getTypeNotice().equals(TYPE_NOTICE.EXEMPLAIRE)) {
                criteresExemp.add(c);
            }
        });
        FacetPage<NoticeV2Solr> noticesWithFacet = noticeV2Repository.findNoticesWithFacetQuery(criteresBiblio, criteresExemp, facettes, facetteFilter, Sort.by(orders), PageRequest.of(page, size));

        return getResultFromQueryFacet(noticesWithFacet, size);
    }

    /**
     * Méthode permettant de construire le json correspondant au résultat de la requête
     *
     * @param noticesWithFacet Listes des notices et des facettes
     * @return le résultat mappé
     */
    private ResultSolr getResultFromQueryFacet(FacetPage<NoticeV2Solr> noticesWithFacet, Integer size) {
        ResultSolr result = new ResultSolr();
        result.setNotices(utilsMapper.mapList(noticesWithFacet.getContent(), Notice.class));
        result.setNbPages(size == 0 ? 1 : (int)Math.ceil((double)noticesWithFacet.getTotalElements() / (double)size));
        result.setNbNotices(noticesWithFacet.getTotalElements());

        List<Page<FacetFieldEntry>> resultFacettes = new ArrayList<>(noticesWithFacet.getFacetResultPages());
        resultFacettes.forEach(f -> {
            if (f.getContent().size() > 0) {
                FacetteSolr facetteSolr = new FacetteSolr(f.getContent().get(0).getKey().getName());
                f.getContent().forEach(field -> {
                    Map<String, Integer> map = new HashMap<>();
                    map.put(field.getValue(), (int) field.getValueCount());
                    facetteSolr.addValeurs(map);
                });
                result.addFacette(facetteSolr);
            }
        });
        return result;
    }
}
