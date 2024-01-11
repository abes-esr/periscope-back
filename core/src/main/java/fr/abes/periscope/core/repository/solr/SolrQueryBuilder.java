package fr.abes.periscope.core.repository.solr;

import fr.abes.periscope.core.criterion.*;
import fr.abes.periscope.core.entity.solr.ItemSolrField;
import fr.abes.periscope.core.entity.solr.NoticeSolrField;
import fr.abes.periscope.core.util.TYPE_NOTICE;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.solr.core.DefaultQueryParser;
import org.springframework.data.solr.core.query.*;

import java.lang.reflect.Field;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;

/**
 * Représente un constructeur de requête SolR pour Periscope V2
 */
@Slf4j
public class SolrQueryBuilder {

    /**
     * Construit la requête SolR à partir des critères de recherche
     *
     * @param criterions Critères de recherche
     * @return Criteria Requête SolR
     */
    public Criteria buildQuery(List<Criterion> criterions) {
        Criteria criteria = null;
        for (Criterion criterion : criterions) {
            // Bloc de critère PCP
            if (criterion instanceof CriterionPcp) {
                Criteria pcpQuery = buildPcpQuery((CriterionPcp) criterion);
                criteria = chainCriteria(criteria, pcpQuery, criterion);
            }

            // Bloc de critère RCR
            if (criterion instanceof CriterionRcr) {
                Criteria rcrQuery = buildRcrQuery((CriterionRcr) criterion);
                criteria = chainCriteria(criteria, rcrQuery, criterion);
            }

            // Bloc de critère Mots du titre
            if (criterion instanceof CriterionTitleWords) {
                Criteria titleWordsQuery = buildTitleWordsQuery((CriterionTitleWords) criterion);
                criteria = chainCriteria(criteria, titleWordsQuery, criterion);

            }

            //Bloc de critère PPN
            if (criterion instanceof CriterionPpn) {
                Criteria ppnQuery = buildPpnQuery((CriterionPpn) criterion);
                criteria = chainCriteria(criteria, ppnQuery, criterion);
            }

            if (criterion instanceof CriterionPpnParent) {
                Criteria ppnParentQuery = buildPpnParentQuery((CriterionPpnParent) criterion);
                criteria = chainCriteria(criteria, ppnParentQuery, criterion);
            }

            //Bloc de critère pays
            if (criterion instanceof CriterionCountry) {
                Criteria countryQuery = buildCountryQuery((CriterionCountry) criterion);
                criteria = chainCriteria(criteria, countryQuery, criterion);
            }

            //Bloc de critère code langue
            if (criterion instanceof CriterionLanguage) {
                Criteria languageQuery = buildLanguageQuery((CriterionLanguage) criterion);
                criteria =  chainCriteria(criteria, languageQuery, criterion);
            }

            //Bloc de critère éditeur
            if (criterion instanceof CriterionEditor) {
                Criteria editorQuery = buildEditorQuery((CriterionEditor) criterion);
                criteria =  chainCriteria(criteria, editorQuery, criterion);
            }

            // bloc de critère ISSN
            if (criterion instanceof CriterionIssn) {
                Criteria issnQuery = buildIssnQuery((CriterionIssn) criterion);
                criteria =  chainCriteria(criteria, issnQuery, criterion);
            }

            //bloc de critère Statut de la bibliothèque
            if (criterion instanceof CriterionStatutBib) {
                Criteria statutBibQuery = buildStatutBibQuery((CriterionStatutBib) criterion);
                criteria =  chainCriteria(criteria, statutBibQuery, criterion);
            }
        }

        return criteria;
    }

    private Criteria chainCriteria(Criteria criteria, Criteria criteriaToChain, Criterion criterion) {
        if (criteria == null) {
            criteria = criteriaToChain;
        }
        else {
            switch (criterion.getBlocOperator()) {
                case LogicalOperator.AND:criteria.and(criteriaToChain);break;
                case LogicalOperator.OR:criteria.or(criteriaToChain);break;
                case LogicalOperator.EXCEPT:criteria.or(criteriaToChain.notOperator());
            }
        }
        return criteria;
    }

    private Criteria buildPpnParentQuery(CriterionPpnParent criterion) {
        Iterator<String> valueIterator = criterion.getPpnParent().iterator();

        Criteria myCriteria;

        String value = valueIterator.next();

        myCriteria = new Criteria(ItemSolrField.PPN_PARENT).is(value);

        // les autres
        while (valueIterator.hasNext()) {
            value = valueIterator.next();
            myCriteria = myCriteria.or(ItemSolrField.PPN_PARENT).is(value);
        }

        return myCriteria.connect();
    }

    /**
     * Construit la requête SolR à partir d'un critère de recherche par PCP
     *
     * @param criterion Les critères de recherche par PCP
     * @return Criteria Requête SolR
     */
    private Criteria buildPcpQuery(CriterionPcp criterion) {

        Iterator<String> pcpIterator = criterion.getPcp().iterator();
        Iterator<String> pcpOperatorIterator = criterion.getPcpOperators().iterator();

        String pcpCode = pcpIterator.next();
        pcpOperatorIterator.next();

        Criteria myCriteria;

        // 1er critère
        myCriteria = (criterion.getTypeNotice().equals(TYPE_NOTICE.BIBLIO) ? Criteria.where(NoticeSolrField.PCP_LIST).is(pcpCode).connect() : Criteria.where(NoticeSolrField.PCP).is(pcpCode).connect());

        Criteria criteria;
        // les autres
        while (pcpOperatorIterator.hasNext()) {
            pcpCode = pcpIterator.next();
            String pcpOperator = pcpOperatorIterator.next();

            switch (pcpOperator) {
                case LogicalOperator.AND:
                    if (criterion.getTypeNotice().equals(TYPE_NOTICE.BIBLIO)) {
                        criteria = Criteria.where(NoticeSolrField.PCP_LIST).is(pcpCode);
                    } else {
                        criteria = Criteria.where(NoticeSolrField.PCP).is(pcpCode);
                    }
                    myCriteria.and(criteria);
                    break;
                case LogicalOperator.OR:
                    if (criterion.getTypeNotice().equals(TYPE_NOTICE.BIBLIO)) {
                        criteria = Criteria.where(NoticeSolrField.PCP_LIST).is(pcpCode);
                    } else {
                        criteria = Criteria.where(NoticeSolrField.PCP).is(pcpCode);
                    }
                    myCriteria.or(criteria);
                    break;
                case LogicalOperator.EXCEPT:
                    if (criterion.getTypeNotice().equals(TYPE_NOTICE.BIBLIO)) {
                        criteria = Criteria.where(NoticeSolrField.PCP_LIST).is(pcpCode).connect().notOperator();
                    } else {
                        criteria = Criteria.where(NoticeSolrField.PCP).is(pcpCode).connect().notOperator();
                    }
                    myCriteria.and(criteria);
                    break;
            }
        }

        return myCriteria.connect();
    }

    /**
     * Construit la requête SolR à partir d'un critère de recherche par RCR
     *
     * @param criterion Les critères de recherche par RCR
     * @return Criteria Requête SolR
     */
    private Criteria buildRcrQuery(CriterionRcr criterion) {

        Iterator<String> rcrIterator = criterion.getRcr().iterator();
        Iterator<String> rcrOperatorIterator = criterion.getRcrOperators().iterator();

        Criteria myCriteria;

        String rcrCode = rcrIterator.next();
        rcrOperatorIterator.next();

        // 1er critère
        myCriteria = (criterion.getTypeNotice().equals(TYPE_NOTICE.BIBLIO) ? new Criteria(NoticeSolrField.RCR_LIST).is(rcrCode).connect() : new Criteria(NoticeSolrField.RCR).is(rcrCode).connect());

        Criteria criteria;
        // les autres
        while (rcrIterator.hasNext()) {
            rcrCode = rcrIterator.next();
            String rcrOperator = rcrOperatorIterator.next();

            switch (rcrOperator) {
                case LogicalOperator.AND:
                    if (criterion.getTypeNotice().equals(TYPE_NOTICE.BIBLIO)) {
                        criteria = new Criteria(NoticeSolrField.RCR_LIST).is(rcrCode);
                    } else {
                        criteria = new Criteria(NoticeSolrField.RCR).is(rcrCode);
                    }
                    myCriteria.and(criteria);
                    break;
                case LogicalOperator.OR:
                    if (criterion.getTypeNotice().equals(TYPE_NOTICE.BIBLIO)) {
                        criteria = new Criteria(NoticeSolrField.RCR_LIST).is(rcrCode);
                    } else {
                        criteria = new Criteria(NoticeSolrField.RCR).is(rcrCode);
                    }
                    myCriteria.or(criteria);
                    break;
                case LogicalOperator.EXCEPT:
                    if (criterion.getTypeNotice().equals(TYPE_NOTICE.BIBLIO)) {
                        criteria = new Criteria(NoticeSolrField.RCR_LIST).is(rcrCode).connect().notOperator();
                    } else {
                        criteria = new Criteria(NoticeSolrField.RCR).is(rcrCode).connect().notOperator();
                    }
                    myCriteria.and(criteria);
                    break;
            }
        }

        return myCriteria.connect();
    }

    /**
     * Construit la requête SolR à partir d'un critère de recherche par mots du titre
     *
     * @param criterion Les critères de recherche par mots du titre
     * @return Criteria Requête SolR
     */
    private Criteria buildTitleWordsQuery(CriterionTitleWords criterion) {

        Iterator<String> valueIterator = criterion.getTitleWords().iterator();
        Iterator<String> operatorIterator = criterion.getTitleWordOperators().iterator();

        Criteria myCriteria;

        String value = valueIterator.next();
        operatorIterator.next();

        //1er critère
        myCriteria = Criteria.where(NoticeSolrField.KEY_TITLE).is(value).
                or(NoticeSolrField.KEY_SHORTED_TITLE).is(value).
                or(NoticeSolrField.PROPER_TITLE).is(value).
                or(NoticeSolrField.TITLE_FROM_DIFFERENT_AUTHOR).is(value).
                or(NoticeSolrField.PARALLEL_TITLE).is(value).
                or(NoticeSolrField.TITLE_COMPLEMENT).is(value).
                or(NoticeSolrField.SECTION_TITLE).is(value).connect();

        // les autres
        while (valueIterator.hasNext()) {
            value = valueIterator.next();
            String titleOperator = operatorIterator.next();

            Criteria criteria;

            switch (titleOperator) {
                case LogicalOperator.AND:
                    criteria = Criteria.where(NoticeSolrField.KEY_TITLE).is(value).
                            or(NoticeSolrField.KEY_SHORTED_TITLE).is(value).
                            or(NoticeSolrField.PROPER_TITLE).is(value).
                            or(NoticeSolrField.TITLE_FROM_DIFFERENT_AUTHOR).is(value).
                            or(NoticeSolrField.PARALLEL_TITLE).is(value).
                            or(NoticeSolrField.TITLE_COMPLEMENT).is(value).
                            or(NoticeSolrField.SECTION_TITLE).is(value).connect();

                    myCriteria.and(criteria);
                    break;
                case LogicalOperator.OR:
                    criteria = Criteria.where(NoticeSolrField.KEY_TITLE).is(value).
                            or(NoticeSolrField.KEY_SHORTED_TITLE).is(value).
                            or(NoticeSolrField.PROPER_TITLE).is(value).
                            or(NoticeSolrField.TITLE_FROM_DIFFERENT_AUTHOR).is(value).
                            or(NoticeSolrField.PARALLEL_TITLE).is(value).
                            or(NoticeSolrField.TITLE_COMPLEMENT).is(value).
                            or(NoticeSolrField.SECTION_TITLE).is(value).connect();

                    myCriteria.or(criteria);
                    break;
                case LogicalOperator.EXCEPT:
                    criteria = Criteria.where(NoticeSolrField.KEY_TITLE).is(value).
                            or(NoticeSolrField.KEY_SHORTED_TITLE).is(value).
                            or(NoticeSolrField.PROPER_TITLE).is(value).
                            or(NoticeSolrField.TITLE_FROM_DIFFERENT_AUTHOR).is(value).
                            or(NoticeSolrField.PARALLEL_TITLE).is(value).
                            or(NoticeSolrField.TITLE_COMPLEMENT).is(value).
                            or(NoticeSolrField.SECTION_TITLE).is(value).connect().notOperator();
                    myCriteria.and(criteria);
                    break;
            }
        }

        return myCriteria.connect();
    }

    /**
     * Construit la requête SolR à partir d'un critère de recherche par code pays
     *
     * @param criterion Les critères de recherche par code pays
     * @return Criteria Requête SolR
     */
    private Criteria buildCountryQuery(CriterionCountry criterion) {

        Iterator<String> countryIterator = criterion.getCountries().iterator();
        Iterator<String> operatorIterator = criterion.getCountryOperators().iterator();

        Criteria myCriteria;

        String countryCode = countryIterator.next();
        operatorIterator.next();

        // 1er critère
        myCriteria = new Criteria(NoticeSolrField.COUNTRY).is(countryCode).connect();

        Criteria criteria;
        // les autres
        while (countryIterator.hasNext()) {
            countryCode = countryIterator.next();
            String countryOperator = operatorIterator.next();

            switch (countryOperator) {
                case LogicalOperator.AND:
                    criteria = new Criteria(NoticeSolrField.COUNTRY).is(countryCode);
                    myCriteria.and(criteria);
                    break;
                case LogicalOperator.OR:
                    criteria = new Criteria(NoticeSolrField.COUNTRY).is(countryCode);
                    myCriteria.or(criteria);
                    break;
                case LogicalOperator.EXCEPT:
                    criteria = new Criteria(NoticeSolrField.COUNTRY).is(countryCode).connect().notOperator();
                    myCriteria.and(criteria);
                    break;
            }
        }

        return myCriteria.connect();
    }

    /**
     * Construit la requête SolR à partir d'un critère de recherche par PPN
     *
     * @param criterion Les critères de recherche par PPN
     * @return Criteria Requête SolR
     */
    private Criteria buildPpnQuery(CriterionPpn criterion) {

        Iterator<String> valueIterator = criterion.getPpn().iterator();

        Criteria myCriteria;

        String value = valueIterator.next();

        myCriteria = new Criteria(NoticeSolrField.PPN).is(value);

        // les autres
        while (valueIterator.hasNext()) {
            value = valueIterator.next();
            myCriteria = myCriteria.or(NoticeSolrField.PPN).is(value);
        }

        return myCriteria.connect();
    }

    /**
     * Construit la requête SolR à partir d'un critère de recherche par code langue
     *
     * @param criterion Les critères de recherche par code langue
     * @return Criteria Requête SolR
     */
    private Criteria buildLanguageQuery(CriterionLanguage criterion) {

        Iterator<String> langueIterator = criterion.getLanguages().iterator();
        Iterator<String> langueOperatorIterator = criterion.getLanguageOperators().iterator();

        Criteria myCriteria;

        String langueCode = langueIterator.next();
        langueOperatorIterator.next();

        myCriteria = new Criteria(NoticeSolrField.LANGUAGE).is(langueCode).connect();
        Criteria criteria;
        // les autres
        while (langueIterator.hasNext()) {
            langueCode = langueIterator.next();
            String langueOperator = langueOperatorIterator.next();

            switch (langueOperator) {
                case LogicalOperator.AND:
                    criteria = new Criteria(NoticeSolrField.LANGUAGE).is(langueCode);
                    myCriteria.and(criteria);
                    break;
                case LogicalOperator.OR:
                    criteria = new Criteria(NoticeSolrField.LANGUAGE).is(langueCode);
                    myCriteria.or(criteria);
                    break;
                case LogicalOperator.EXCEPT:
                    criteria = new Criteria(NoticeSolrField.LANGUAGE).is(langueCode).connect().notOperator();
                    myCriteria.and(criteria);
                    break;
            }
        }

        return myCriteria.connect();
    }

    /**
     * Construit la requête SolR à partir d'un critère de recherche par éditeur
     *
     * @param criterion Les critères de recherche par editeur
     * @return Criteria Requête SolR
     */
    private Criteria buildEditorQuery(CriterionEditor criterion) {

        Iterator<String> editorIteror = criterion.getEditors().iterator();
        Iterator<String> editorOperatorIterator = criterion.getEditorOperators().iterator();

        Criteria myCriteria;

        String editor = editorIteror.next();
        editorOperatorIterator.next();

        // 1er critère

        myCriteria = new Criteria(NoticeSolrField.EDITOR).is(editor).connect();
        Criteria criteria;
        // les autres
        while (editorIteror.hasNext()) {
            editor = editorIteror.next();
            String editorOperator = editorOperatorIterator.next();

            switch (editorOperator) {
                case LogicalOperator.AND:
                    criteria = new Criteria(NoticeSolrField.EDITOR).is(editor).connect();
                    myCriteria.and(criteria);
                    break;
                case LogicalOperator.OR:
                    criteria = new Criteria(NoticeSolrField.EDITOR).is(editor).connect();
                    myCriteria.or(criteria);
                    break;
                case LogicalOperator.EXCEPT:
                    criteria = new Criteria(NoticeSolrField.EDITOR).is(editor).connect().notOperator();
                    myCriteria.and(criteria);
                    break;
            }
        }

        return myCriteria.connect();
    }

    /**
     * Construit la requête SolR à partir d'un critère de recherche par ISSN
     *
     * @param criterion Les critères de recherche par ISSN
     * @return Criteria Requête SolR
     */
    private Criteria buildIssnQuery(CriterionIssn criterion) {

        Iterator<String> issnIterator = criterion.getIssn().iterator();

        Criteria myCriteria;

        String value = issnIterator.next();
        myCriteria = new Criteria(NoticeSolrField.ISSN).is(value);

        // les autres
        while (issnIterator.hasNext()) {
            value = issnIterator.next();
            myCriteria = myCriteria.or(NoticeSolrField.ISSN).is(value);
        }

        return myCriteria.connect();
    }

    /**
     * Construit la requête SolR à partir d'un critère de recherche par Statut de bibliothèque
     *
     * @param criterion Le critère de recherche par statut de bibliothèque
     * @return Criteria Requête SolR
     */
    private Criteria buildStatutBibQuery(CriterionStatutBib criterion) {
        Iterator<String> statutIterator = criterion.getStatutBibliotheque().iterator();
        Iterator<String> statutOperatorIterator = criterion.getStatutOperators().iterator();

        Criteria myCriteria;

        String statut = statutIterator.next();
        statutOperatorIterator.next();

        // 1er critère
        myCriteria = new Criteria(NoticeSolrField.STATUT_LIST).is(statut).connect();

        Criteria criteria;
        // les autres
        while (statutIterator.hasNext()) {
            statut = statutIterator.next();
            String statutOperator = statutOperatorIterator.next();

            switch (statutOperator) {
                case LogicalOperator.AND:
                    criteria = new Criteria(NoticeSolrField.STATUT_LIST).is(statut);
                    myCriteria.and(criteria);
                    break;
                case LogicalOperator.OR:
                    criteria = new Criteria(NoticeSolrField.STATUT_LIST).is(statut);
                    myCriteria.or(criteria);
                    break;
                case LogicalOperator.EXCEPT:
                    criteria = new Criteria(NoticeSolrField.STATUT_LIST).is(statut).connect().notOperator();
                    myCriteria.and(criteria);
                    break;
            }
        }
        return myCriteria.connect();
    }


    /**
     * Méthode permettant d'ajouter une liste de facette sur des zones de la notices biblio à la requête
     *
     * @param query  : requête sur laquelle ajouter les facettes
     * @param facets : liste générale des zones de facettes (biblio + exemplaire)
     * @return requête mise à jour
     */
    public FacetQuery addFacetsNotices(FacetQuery query, List<String> facets) {
        FacetOptions options = new FacetOptions();
        facets.forEach(f -> {
            //cas ou la facette est une zone de la notice bibliographique
            Arrays.stream(NoticeSolrField.class.getFields()).forEach(field -> {
                if (field.getName().toLowerCase(Locale.ROOT).equals(f.toLowerCase(Locale.ROOT))) {
                    try {
                        options.addFacetOnField((String) field.get(null));
                    } catch (IllegalAccessException e) {
                        log.error("Impossible d'accéder à la facette " + field.getName());
                    }
                }
            });
            if (options.hasFields()) {
                query.setFacetOptions(options);
            }
        });
        return query;
    }

    /**
     * Méthode permettant de générer une chaine à concaténer à une requête correspondant aux facettes d'exemplaires
     *
     * @param facets liste générale des zones de facettes (biblio + exemplaire)
     * @return la chaine à concaténer à la requête
     */
    public String addFacetsExemplaires(List<String> facets) {
        String queryFacet = "";
        DefaultQueryParser dqp = new DefaultQueryParser(null);
        Iterator<String> itFacet = facets.iterator();
        while (itFacet.hasNext()) {
            String f = itFacet.next();
            //cas ou la facette est une zone d'exemplaire
            Iterator<Field> it = Arrays.asList(ItemSolrField.class.getFields()).iterator();
            while (it.hasNext()) {
                Field solrField = it.next();
                if (solrField.getName().equals(f)) {
                    try {
                        queryFacet += "&child.facet.field=" + solrField.get(null);
                    } catch (IllegalAccessException e) {
                        log.error("Impossible d'accéder à la facette " + solrField.getName());
                    }
                }
            }
        }
        return queryFacet;
    }

    public FacetQuery constructFacetQuery(List<Criterion> criteriaNotice, List<Criterion> criteriaExemp) {
        SimpleQuery parent = new SimpleQuery();
        if (!criteriaExemp.isEmpty()) {
            SimpleQuery solrQuery = new SimpleQuery(buildQuery(criteriaExemp));
            DefaultQueryParser dqp = new DefaultQueryParser(null);
            String actualQuery = dqp.getQueryString(solrQuery, null);
            parent = new SimpleQuery("{!parent which=notice_type:notice}" + actualQuery);
        }
        FacetQuery query = new SimpleFacetQuery();
        if (parent.getCriteria() != null) {
            query = new SimpleFacetQuery(parent.getCriteria());
            if (!criteriaNotice.isEmpty()) {
                query.addFilterQuery(new SimpleFilterQuery(buildQuery(criteriaNotice)));
            }
        } else {
            query.addCriteria(buildQuery(criteriaNotice));
        }
        return query;
    }

    public FacetQuery addFacetsFilters(FacetQuery query, List<CriterionFacette> facetteFilter) {
        if (!facetteFilter.isEmpty()) {
            Iterator<CriterionFacette> fqIt = facetteFilter.iterator();
            while (fqIt.hasNext()) {
                CriterionFacette facette = fqIt.next();
                Arrays.stream(NoticeSolrField.class.getFields()).forEach(field -> {
                    String f = facette.getZone().toLowerCase(Locale.ROOT);
                    if (field.getName().toLowerCase(Locale.ROOT).equals(f)) {
                        query.addFilterQuery(new SimpleFilterQuery(new Criteria(f).is(facette.getValeurs())));
                    }
                });

            }
        }
        return query;
    }
}
