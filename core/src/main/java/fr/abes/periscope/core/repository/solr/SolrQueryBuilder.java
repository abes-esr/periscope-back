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
     * @param criteria Critères de recherche
     * @return Criteria Requête SolR
     */
    public Criteria buildQuery(List<Criterion> criteria) {
        FilterQuery filterQuery = new SimpleFilterQuery();

        criteria.stream().forEach(criterion -> {

            // Bloc de critère PCP
            if (criterion instanceof CriterionPcp) {
                Criteria pcpQuery = buildPcpQuery((CriterionPcp) criterion);
                filterQuery.addCriteria(pcpQuery);
            }

            // Bloc de critère RCR
            if (criterion instanceof CriterionRcr) {
                Criteria rcrQuery = buildRcrQuery((CriterionRcr) criterion);
                filterQuery.addCriteria(rcrQuery);
            }

            // Bloc de critère Mots du titre
            if (criterion instanceof CriterionTitleWords) {
                Criteria titleWordsQuery = buildTitleWordsQuery((CriterionTitleWords) criterion);
                filterQuery.addCriteria(titleWordsQuery);
            }

            //Bloc de critère PPN
            if (criterion instanceof CriterionPpn) {
                Criteria ppnQuery = buildPpnQuery((CriterionPpn) criterion);
                filterQuery.addCriteria(ppnQuery);
            }

            if (criterion instanceof CriterionPpnParent) {
                Criteria ppnParentQuery = buildPpnParentQuery((CriterionPpnParent) criterion);
                filterQuery.addCriteria(ppnParentQuery);
            }

            //Bloc de critère pays
            if (criterion instanceof CriterionCountry) {
                Criteria countryQuery = buildCountryQuery((CriterionCountry) criterion);
                filterQuery.addCriteria(countryQuery);
            }

            //Bloc de critère code langue
            if (criterion instanceof CriterionLanguage) {
                Criteria languageQuery = buildLanguageQuery((CriterionLanguage) criterion);
                filterQuery.addCriteria(languageQuery);
            }

            //Bloc de critère éditeur
            if (criterion instanceof CriterionEditor) {
                Criteria countryQuery = buildEditorQuery((CriterionEditor) criterion);
                filterQuery.addCriteria(countryQuery);
            }

            // bloc de critère ISSN
            if (criterion instanceof CriterionIssn) {
                Criteria issnQuery = buildIssnQuery((CriterionIssn) criterion);
                filterQuery.addCriteria(issnQuery);
            }

            //bloc de critère Statut de la bibliothèque
            if (criterion instanceof CriterionStatutBib) {
                Criteria statutBibQuery = buildStatutBibQuery((CriterionStatutBib) criterion);
                filterQuery.addCriteria(statutBibQuery);
            }
        });

        return filterQuery.getCriteria();
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

        return getBlocOperator(criterion, myCriteria);
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
        String pcpOperator = pcpOperatorIterator.next();

        Criteria myCriteria;

        // 1er critère
        switch (pcpOperator) {
            case LogicalOperator.EXCEPT:
                myCriteria = (criterion.getTypeNotice().equals(TYPE_NOTICE.BIBLIO) ? new Criteria(NoticeSolrField.PCP_LIST).is(pcpCode).not() : new Criteria(NoticeSolrField.PCP).is(pcpCode).not());
                break;
            default:
                myCriteria = (criterion.getTypeNotice().equals(TYPE_NOTICE.BIBLIO) ? new Criteria(NoticeSolrField.PCP_LIST).is(pcpCode) : new Criteria(NoticeSolrField.PCP).is(pcpCode));
                break;
        }

        // les autres
        while (pcpOperatorIterator.hasNext()) {
            pcpCode = pcpIterator.next();
            pcpOperator = pcpOperatorIterator.next();

            switch (pcpOperator) {
                case LogicalOperator.AND:
                    myCriteria = (criterion.getTypeNotice().equals(TYPE_NOTICE.BIBLIO) ? myCriteria.and(NoticeSolrField.PCP_LIST).is(pcpCode) : myCriteria.and(NoticeSolrField.PCP).is(pcpCode));
                    break;
                case LogicalOperator.OR:
                    myCriteria = (criterion.getTypeNotice().equals(TYPE_NOTICE.BIBLIO) ? myCriteria.or(NoticeSolrField.PCP_LIST).is(pcpCode) : myCriteria.or(NoticeSolrField.PCP).is(pcpCode));
                    break;
                case LogicalOperator.EXCEPT:
                    myCriteria = (criterion.getTypeNotice().equals(TYPE_NOTICE.BIBLIO) ? myCriteria.or(NoticeSolrField.PCP_LIST).is(pcpCode).not() : myCriteria.or(NoticeSolrField.PCP).is(pcpCode).not());
                    break;
            }
        }

        return getBlocOperator(criterion, myCriteria);
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
        String rcrOperator = rcrOperatorIterator.next();

        // 1er critère
        switch (rcrOperator) {
            case LogicalOperator.EXCEPT:
                myCriteria = (criterion.getTypeNotice().equals(TYPE_NOTICE.BIBLIO) ? new Criteria(NoticeSolrField.RCR_LIST).is(rcrCode).not() : new Criteria(NoticeSolrField.RCR).is(rcrCode).not());
                break;
            default:
                myCriteria = (criterion.getTypeNotice().equals(TYPE_NOTICE.BIBLIO) ? new Criteria(NoticeSolrField.RCR_LIST).is(rcrCode) : new Criteria(NoticeSolrField.RCR).is(rcrCode));
                break;
        }

        // les autres
        while (rcrIterator.hasNext()) {
            rcrCode = rcrIterator.next();
            rcrOperator = rcrOperatorIterator.next();

            switch (rcrOperator) {
                case LogicalOperator.AND:
                    myCriteria = (criterion.getTypeNotice().equals(TYPE_NOTICE.BIBLIO) ? myCriteria.and(NoticeSolrField.RCR_LIST).is(rcrCode) : myCriteria.and(NoticeSolrField.RCR).is(rcrCode));
                    break;
                case LogicalOperator.OR:
                    myCriteria = (criterion.getTypeNotice().equals(TYPE_NOTICE.BIBLIO) ? myCriteria.or(NoticeSolrField.RCR_LIST).is(rcrCode) : myCriteria.or(NoticeSolrField.RCR));
                    break;
                case LogicalOperator.EXCEPT:
                    myCriteria = (criterion.getTypeNotice().equals(TYPE_NOTICE.BIBLIO) ? myCriteria.or(NoticeSolrField.RCR_LIST).is(rcrCode).not() : myCriteria.or(NoticeSolrField.RCR).is(rcrCode).not());
                    break;
            }
        }

        return getBlocOperator(criterion, myCriteria);
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
        String operator = operatorIterator.next();

        // 1er critère
        switch (operator) {
            case LogicalOperator.EXCEPT:
                myCriteria = new Criteria(NoticeSolrField.KEY_TITLE).is(value).not().
                        or(NoticeSolrField.KEY_SHORTED_TITLE).is(value).not().
                        or(NoticeSolrField.PROPER_TITLE).is(value).not().
                        or(NoticeSolrField.TITLE_FROM_DIFFERENT_AUTHOR).is(value).not().
                        or(NoticeSolrField.PARALLEL_TITLE).is(value).not().
                        or(NoticeSolrField.TITLE_COMPLEMENT).is(value).not().
                        or(NoticeSolrField.SECTION_TITLE).is(value).not().connect();
                break;
            default:
                myCriteria = new Criteria(NoticeSolrField.KEY_TITLE).is(value).
                        or(NoticeSolrField.KEY_SHORTED_TITLE).is(value).
                        or(NoticeSolrField.PROPER_TITLE).is(value).
                        or(NoticeSolrField.TITLE_FROM_DIFFERENT_AUTHOR).is(value).
                        or(NoticeSolrField.PARALLEL_TITLE).is(value).
                        or(NoticeSolrField.TITLE_COMPLEMENT).is(value).
                        or(NoticeSolrField.SECTION_TITLE).is(value).connect();
                break;
        }

        // les autres
        while (valueIterator.hasNext()) {
            value = valueIterator.next();
            operator = operatorIterator.next();

            Criteria criteria = null;

            switch (operator) {
                case LogicalOperator.AND:
                    criteria = new Criteria(NoticeSolrField.KEY_TITLE).is(value).
                            or(NoticeSolrField.KEY_SHORTED_TITLE).is(value).
                            or(NoticeSolrField.PROPER_TITLE).is(value).
                            or(NoticeSolrField.TITLE_FROM_DIFFERENT_AUTHOR).is(value).
                            or(NoticeSolrField.PARALLEL_TITLE).is(value).
                            or(NoticeSolrField.TITLE_COMPLEMENT).is(value).
                            or(NoticeSolrField.SECTION_TITLE).is(value).connect();

                    myCriteria.and(criteria);
                    break;
                case LogicalOperator.OR:
                    criteria = new Criteria(NoticeSolrField.KEY_TITLE).is(value).
                            or(NoticeSolrField.KEY_SHORTED_TITLE).is(value).
                            or(NoticeSolrField.PROPER_TITLE).is(value).
                            or(NoticeSolrField.TITLE_FROM_DIFFERENT_AUTHOR).is(value).
                            or(NoticeSolrField.PARALLEL_TITLE).is(value).
                            or(NoticeSolrField.TITLE_COMPLEMENT).is(value).
                            or(NoticeSolrField.SECTION_TITLE).is(value).connect();

                    myCriteria.or(criteria);
                    break;
                case LogicalOperator.EXCEPT:
                    criteria = new Criteria(NoticeSolrField.KEY_TITLE).is(value).not().
                            or(NoticeSolrField.KEY_SHORTED_TITLE).is(value).not().
                            or(NoticeSolrField.PROPER_TITLE).is(value).not().
                            or(NoticeSolrField.TITLE_FROM_DIFFERENT_AUTHOR).is(value).not().
                            or(NoticeSolrField.PARALLEL_TITLE).is(value).not().
                            or(NoticeSolrField.TITLE_COMPLEMENT).is(value).not().
                            or(NoticeSolrField.SECTION_TITLE).is(value).not().connect();

                    myCriteria.or(criteria).not();
                    break;
            }
        }

        return getBlocOperator(criterion, myCriteria);
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
        String operator = operatorIterator.next();

        // 1er critère
        switch (operator) {
            case LogicalOperator.EXCEPT:
                myCriteria = new Criteria(NoticeSolrField.COUNTRY).is(countryCode).not().connect();
                break;
            default:
                myCriteria = new Criteria(NoticeSolrField.COUNTRY).is(countryCode).connect();
                break;
        }

        // les autres
        while (countryIterator.hasNext()) {
            countryCode = countryIterator.next();
            operator = operatorIterator.next();

            switch (operator) {
                case LogicalOperator.AND:
                    myCriteria = myCriteria.connect().and(NoticeSolrField.COUNTRY).is(countryCode);
                    break;
                case LogicalOperator.OR:
                    myCriteria = myCriteria.connect().or(NoticeSolrField.COUNTRY).is(countryCode);
                    break;
                case LogicalOperator.EXCEPT:
                    myCriteria = myCriteria.connect().or(NoticeSolrField.COUNTRY).is(countryCode).not();
                    break;
            }
        }

        return getBlocOperator(criterion, myCriteria);
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

        return getBlocOperator(criterion, myCriteria);
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
        String langueOperator = langueOperatorIterator.next();

        // 1er critère
        switch (langueOperator) {
            case LogicalOperator.EXCEPT:
                myCriteria = new Criteria(NoticeSolrField.LANGUAGE).is(langueCode).not().connect();
                break;
            default:
                myCriteria = new Criteria(NoticeSolrField.LANGUAGE).is(langueCode).connect();
                break;
        }

        // les autres
        while (langueIterator.hasNext()) {
            langueCode = langueIterator.next();
            langueOperator = langueOperatorIterator.next();

            switch (langueOperator) {
                case LogicalOperator.AND:
                    myCriteria = myCriteria.connect().and(NoticeSolrField.LANGUAGE).is(langueCode);
                    break;
                case LogicalOperator.OR:
                    myCriteria = myCriteria.connect().or(NoticeSolrField.LANGUAGE).is(langueCode);
                    break;
                case LogicalOperator.EXCEPT:
                    myCriteria = myCriteria.connect().or(NoticeSolrField.LANGUAGE).is(langueCode).not();
                    break;
            }
        }

        return getBlocOperator(criterion, myCriteria);
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
        String editorOperator = editorOperatorIterator.next();

        // 1er critère
        switch (editorOperator) {
            case LogicalOperator.EXCEPT:
                myCriteria = new Criteria(NoticeSolrField.EDITOR).is(editor).not().connect();
                break;
            default:
                myCriteria = new Criteria(NoticeSolrField.EDITOR).is(editor).connect();
                break;
        }

        // les autres
        while (editorIteror.hasNext()) {
            editor = editorIteror.next();
            editorOperator = editorOperatorIterator.next();

            switch (editorOperator) {
                case LogicalOperator.AND:
                    myCriteria = myCriteria.connect().and(NoticeSolrField.EDITOR).is(editor);
                    break;
                case LogicalOperator.OR:
                    myCriteria = myCriteria.connect().or(NoticeSolrField.EDITOR).is(editor);
                    break;
                case LogicalOperator.EXCEPT:
                    myCriteria = myCriteria.connect().or(NoticeSolrField.EDITOR).is(editor).not();
                    break;
            }
        }

        return getBlocOperator(criterion, myCriteria);
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

        return getBlocOperator(criterion, myCriteria);
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
        String statutOperator = statutOperatorIterator.next();

        // 1er critère
        switch (statutOperator) {
            case LogicalOperator.EXCEPT:
                myCriteria = new Criteria(NoticeSolrField.STATUT_LIST).is(statut).not().connect();
                break;
            default:
                myCriteria = new Criteria(NoticeSolrField.STATUT_LIST).is(statut).connect();
                break;
        }

        // les autres
        while (statutIterator.hasNext()) {
            statut = statutIterator.next();
            statutOperator = statutOperatorIterator.next();

            switch (statutOperator) {
                case LogicalOperator.AND:
                    myCriteria = myCriteria.connect().and(NoticeSolrField.STATUT_LIST).is(statut);
                    break;
                case LogicalOperator.OR:
                    myCriteria = myCriteria.connect().or(NoticeSolrField.STATUT_LIST).is(statut);
                    break;
                case LogicalOperator.EXCEPT:
                    myCriteria = myCriteria.connect().or(NoticeSolrField.STATUT_LIST).is(statut).not();
                    break;
            }
        }
        return getBlocOperator(criterion, myCriteria);
    }

    private Criteria getBlocOperator(Criterion criterion, Criteria myCriteria) {
        switch (criterion.getBlocOperator()) {
            case LogicalOperator.AND:
                break;
            case LogicalOperator.OR:
                myCriteria.setPartIsOr(true);
                break;
            case LogicalOperator.EXCEPT:
                myCriteria = myCriteria.notOperator();
                break;
        }

        return myCriteria;
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
