package fr.abes.periscope.core.repository.solr.v2;

import fr.abes.periscope.core.criterion.*;
import fr.abes.periscope.core.entity.v2.solr.ItemSolrField;
import fr.abes.periscope.core.entity.v2.solr.NoticeV2SolrField;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Pageable;
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

        Iterator<Criterion> criteriaIterator = criteria.iterator();
        while (criteriaIterator.hasNext()) {
            Criterion criterion = criteriaIterator.next();

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
        }

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

        // pour le bloc entier
        switch (criterion.getBlocOperator()) {
            case LogicalOperator.AND:
                myCriteria = myCriteria.connect();
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
     * Construit la requête SolR à partir d'un critère de recherche par PCP
     *
     * @param criterion Les critères de recherche par PCP
     * @return Criteria Requête SolR
     */
    private Criteria buildPcpQuery(CriterionPcp criterion) {

        Iterator<String> pcpIterator = criterion.getPcp().iterator();
        String pcpCode = pcpIterator.next();

        Criteria myCriteria = new Criteria(NoticeV2SolrField.PCP_LIST).is(pcpCode);

        while (pcpIterator.hasNext()) {
            pcpCode = pcpIterator.next();
            myCriteria = myCriteria.or(NoticeV2SolrField.PCP_LIST).is(pcpCode);
        }

        switch (criterion.getBlocOperator()) {
            case LogicalOperator.AND:
                // AND par défaut, on ne fait rien
                break;
            case LogicalOperator.OR:
                myCriteria.setPartIsOr(true);
                break;
            case LogicalOperator.EXCEPT:
                myCriteria = myCriteria.notOperator();
                break;
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

        Criteria myCriteria = null;

        String rcrCode = rcrIterator.next();
        String rcrOperator = rcrOperatorIterator.next();

        // 1er critère
        switch (rcrOperator) {
            case LogicalOperator.EXCEPT:
                myCriteria = new Criteria(NoticeV2SolrField.RCR_LIST).is(rcrCode).not();
                break;
            default:
                myCriteria = new Criteria(NoticeV2SolrField.RCR_LIST).is(rcrCode);
                break;
        }

        // les autres
        while (rcrIterator.hasNext()) {
            rcrCode = rcrIterator.next();
            rcrOperator = rcrOperatorIterator.next();

            switch (rcrOperator) {
                case LogicalOperator.AND:
                    myCriteria = myCriteria.and(NoticeV2SolrField.RCR_LIST).is(rcrCode);
                    break;
                case LogicalOperator.OR:
                    myCriteria = myCriteria.or(NoticeV2SolrField.RCR_LIST).is(rcrCode);
                    break;
                case LogicalOperator.EXCEPT:
                    myCriteria = myCriteria.and(NoticeV2SolrField.RCR_LIST).is(rcrCode).not();
                    break;
            }
        }

        // pour le bloc entier
        switch (criterion.getBlocOperator()) {
            case LogicalOperator.AND:
                myCriteria = myCriteria.connect();
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
     * Construit la requête SolR à partir d'un critère de recherche par mots du titre
     *
     * @param criterion Les critères de recherche par mots du titre
     * @return Criteria Requête SolR
     */
    private Criteria buildTitleWordsQuery(CriterionTitleWords criterion) {

        Iterator<String> valueIterator = criterion.getTitleWords().iterator();
        Iterator<String> operatorIterator = criterion.getTitleWordOperators().iterator();

        Criteria myCriteria = null;

        String value = valueIterator.next();
        String operator = operatorIterator.next();

        // 1er critère
        switch (operator) {
            case LogicalOperator.EXCEPT:
                myCriteria = new Criteria(NoticeV2SolrField.KEY_TITLE).is(value).not().
                        or(NoticeV2SolrField.KEY_SHORTED_TITLE).is(value).not().
                        or(NoticeV2SolrField.PROPER_TITLE).is(value).not().
                        or(NoticeV2SolrField.TITLE_FROM_DIFFERENT_AUTHOR).is(value).not().
                        or(NoticeV2SolrField.PARALLEL_TITLE).is(value).not().
                        or(NoticeV2SolrField.TITLE_COMPLEMENT).is(value).not().
                        or(NoticeV2SolrField.SECTION_TITLE).is(value).not().connect();
                break;
            default:
                myCriteria = new Criteria(NoticeV2SolrField.KEY_TITLE).is(value).
                        or(NoticeV2SolrField.KEY_SHORTED_TITLE).is(value).
                        or(NoticeV2SolrField.PROPER_TITLE).is(value).
                        or(NoticeV2SolrField.TITLE_FROM_DIFFERENT_AUTHOR).is(value).
                        or(NoticeV2SolrField.PARALLEL_TITLE).is(value).
                        or(NoticeV2SolrField.TITLE_COMPLEMENT).is(value).
                        or(NoticeV2SolrField.SECTION_TITLE).is(value).connect();
                break;
        }

        // les autres
        while (valueIterator.hasNext()) {
            value = valueIterator.next();
            operator = operatorIterator.next();

            switch (operator) {
                case LogicalOperator.AND:
                    myCriteria = myCriteria.connect().and(NoticeV2SolrField.KEY_TITLE).is(value).
                            or(NoticeV2SolrField.KEY_SHORTED_TITLE).is(value).
                            or(NoticeV2SolrField.PROPER_TITLE).is(value).
                            or(NoticeV2SolrField.TITLE_FROM_DIFFERENT_AUTHOR).is(value).
                            or(NoticeV2SolrField.PARALLEL_TITLE).is(value).
                            or(NoticeV2SolrField.TITLE_COMPLEMENT).is(value).
                            or(NoticeV2SolrField.SECTION_TITLE).is(value);
                    break;
                case LogicalOperator.OR:
                    myCriteria = myCriteria.connect().or(NoticeV2SolrField.KEY_TITLE).is(value).
                            or(NoticeV2SolrField.KEY_SHORTED_TITLE).is(value).
                            or(NoticeV2SolrField.PROPER_TITLE).is(value).
                            or(NoticeV2SolrField.TITLE_FROM_DIFFERENT_AUTHOR).is(value).
                            or(NoticeV2SolrField.PARALLEL_TITLE).is(value).
                            or(NoticeV2SolrField.TITLE_COMPLEMENT).is(value).
                            or(NoticeV2SolrField.SECTION_TITLE).is(value);
                    break;
                case LogicalOperator.EXCEPT:
                    myCriteria = myCriteria.connect().and(NoticeV2SolrField.KEY_TITLE).is(value).not().
                            or(NoticeV2SolrField.KEY_SHORTED_TITLE).is(value).not().
                            or(NoticeV2SolrField.PROPER_TITLE).is(value).not().
                            or(NoticeV2SolrField.TITLE_FROM_DIFFERENT_AUTHOR).is(value).not().
                            or(NoticeV2SolrField.PARALLEL_TITLE).is(value).not().
                            or(NoticeV2SolrField.TITLE_COMPLEMENT).is(value).not().
                            or(NoticeV2SolrField.SECTION_TITLE).is(value).not();
                    break;
            }
        }

        // pour le bloc entier
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
     * Construit la requête SolR à partir d'un critère de recherche par code pays
     *
     * @param criterion Les critères de recherche par code pays
     * @return Criteria Requête SolR
     */
    private Criteria buildCountryQuery(CriterionCountry criterion) {

        Iterator<String> valueIterator = criterion.getCountries().iterator();
        Iterator<String> operatorIterator = criterion.getCountryOperators().iterator();

        Criteria myCriteria = null;

        String value = valueIterator.next();
        String operator = operatorIterator.next();

        // 1er critère
        switch (operator) {
            case LogicalOperator.EXCEPT:
                myCriteria = new Criteria(NoticeV2SolrField.COUNTRY).is(value).not().connect().and(NoticeV2SolrField.KEY_TITLE);
                break;
            default:
                myCriteria = new Criteria(NoticeV2SolrField.COUNTRY).is(value).connect().and(NoticeV2SolrField.KEY_TITLE);
                break;
        }

        // les autres
        while (valueIterator.hasNext()) {
            value = valueIterator.next();
            operator = operatorIterator.next();

            switch (operator) {
                case LogicalOperator.AND:
                    myCriteria = myCriteria.connect().and(NoticeV2SolrField.COUNTRY).is(value).and(NoticeV2SolrField.KEY_TITLE);
                    break;
                case LogicalOperator.OR:
                    myCriteria = myCriteria.connect().or(NoticeV2SolrField.COUNTRY).is(value).and(NoticeV2SolrField.KEY_TITLE);
                    break;
                case LogicalOperator.EXCEPT:
                    myCriteria = myCriteria.connect().and(NoticeV2SolrField.COUNTRY).is(value).not().and(NoticeV2SolrField.KEY_TITLE);
                    break;
            }
        }

        // pour le bloc entier
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
     * Construit la requête SolR à partir d'un critère de recherche par PPN
     *
     * @param criterion Les critères de recherche par PPN
     * @return Criteria Requête SolR
     */
    private Criteria buildPpnQuery(CriterionPpn criterion) {

        Iterator<String> valueIterator = criterion.getPpn().iterator();

        Criteria myCriteria;

        String value = valueIterator.next();

        myCriteria = new Criteria(NoticeV2SolrField.PPN).is(value);

        // les autres
        while (valueIterator.hasNext()) {
            value = valueIterator.next();
            myCriteria = myCriteria.or(NoticeV2SolrField.PPN).is(value);
        }

        // pour le bloc entier
        switch (criterion.getBlocOperator()) {
            case LogicalOperator.AND:
                myCriteria = myCriteria.connect();
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
     * Construit la requête SolR à partir d'un critère de recherche par code langue
     *
     * @param criterion Les critères de recherche par code langue
     * @return Criteria Requête SolR
     */
    private Criteria buildLanguageQuery(CriterionLanguage criterion) {

        Iterator<String> langueIterator = criterion.getLanguages().iterator();
        Iterator<String> langueOperatorIterator = criterion.getLanguageOperators().iterator();

        Criteria myCriteria = null;

        String langueCode = langueIterator.next();
        String langueOperator = langueOperatorIterator.next();

        // 1er critère
        switch (langueOperator) {
            case LogicalOperator.EXCEPT:
                myCriteria = new Criteria(NoticeV2SolrField.LANGUAGE).is(langueCode).not().connect();
                break;
            default:
                myCriteria = new Criteria(NoticeV2SolrField.LANGUAGE).is(langueCode).connect();
                break;
        }

        // les autres
        while (langueIterator.hasNext()) {
            langueCode = langueIterator.next();
            langueOperator = langueOperatorIterator.next();

            switch (langueOperator) {
                case LogicalOperator.AND:
                    myCriteria = myCriteria.connect().and(NoticeV2SolrField.LANGUAGE).is(langueCode);
                    break;
                case LogicalOperator.OR:
                    myCriteria = myCriteria.connect().or(NoticeV2SolrField.LANGUAGE).is(langueCode);
                    break;
                case LogicalOperator.EXCEPT:
                    myCriteria = myCriteria.connect().and(NoticeV2SolrField.LANGUAGE).is(langueCode).not();
                    break;
            }
        }
        myCriteria = myCriteria.and(NoticeV2SolrField.KEY_TITLE);

        // pour le bloc entier
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
     * Construit la requête SolR à partir d'un critère de recherche par éditeur
     *
     * @param criterion Les critères de recherche par editeur
     * @return Criteria Requête SolR
     */
    private Criteria buildEditorQuery(CriterionEditor criterion) {

        Iterator<String> editorIteror = criterion.getEditors().iterator();
        Iterator<String> editorOperatorIterator = criterion.getEditorOperators().iterator();

        Criteria myCriteria = null;

        String editor = editorIteror.next();
        String editorOperator = editorOperatorIterator.next();

        // 1er critère
        switch (editorOperator) {
            case LogicalOperator.EXCEPT:
                myCriteria = new Criteria(NoticeV2SolrField.EDITOR).is(editor).not().connect();
                break;
            default:
                myCriteria = new Criteria(NoticeV2SolrField.EDITOR).is(editor).connect();
                break;
        }

        // les autres
        while (editorIteror.hasNext()) {
            editor = editorIteror.next();
            editorOperator = editorOperatorIterator.next();

            switch (editorOperator) {
                case LogicalOperator.AND:
                    myCriteria = myCriteria.connect().and(NoticeV2SolrField.EDITOR).is(editor);
                    break;
                case LogicalOperator.OR:
                    myCriteria = myCriteria.connect().or(NoticeV2SolrField.EDITOR).is(editor);
                    break;
                case LogicalOperator.EXCEPT:
                    myCriteria = myCriteria.connect().and(NoticeV2SolrField.EDITOR).is(editor).not();
                    break;
            }
        }

        // pour le bloc entier
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
     * Construit la requête SolR à partir d'un critère de recherche par ISSN
     *
     * @param criterion Les critères de recherche par ISSN
     * @return Criteria Requête SolR
     */
    private Criteria buildIssnQuery(CriterionIssn criterion) {

        Iterator<String> issnIterator = criterion.getIssn().iterator();

        Criteria myCriteria;

        String value = issnIterator.next();
        myCriteria = new Criteria(NoticeV2SolrField.ISSN).is(value);

        // les autres
        while (issnIterator.hasNext()) {
            value = issnIterator.next();
            myCriteria = myCriteria.or(NoticeV2SolrField.ISSN).is(value);
        }

        // pour le bloc entier
        switch (criterion.getBlocOperator()) {
            case LogicalOperator.AND:
                myCriteria = myCriteria.connect();
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
        Iterator<String> itFacet = facets.iterator();
        while (itFacet.hasNext()) {
            String f = itFacet.next();
            //cas ou la facette est une zone de la notice bibliographique
            Arrays.stream(NoticeV2SolrField.class.getFields()).forEach(field -> {
                if (field.getName().toLowerCase(Locale.ROOT).equals(f.toLowerCase(Locale.ROOT))) {
                    try {
                        options.addFacetOnField((String) field.get(null));
                    } catch (IllegalAccessException e) {
                        e.printStackTrace();
                    }
                }
            });
            if (options.hasFields()) {
                query.setFacetOptions(options);
            }
        }
        return query;
    }

    /**
     * Méthode permettant de générer une chaine à concaténer à une requête correspondant aux facettes d'exemplaires
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
                        queryFacet +=  "&child.facet.field=" + solrField.get(null);
                    } catch (IllegalAccessException e) {
                        e.printStackTrace();
                    }
                }
            }
        }
        return queryFacet;
    }

    public FacetQuery constructFacetQuery(List<Criterion> criteriaNotice, List<Criterion> criteriaExemp, Pageable page) {
        SimpleQuery parent = new SimpleQuery();
        if (!criteriaExemp.isEmpty()) {
            SimpleQuery solrQuery = new SimpleQuery(buildQuery(criteriaExemp), page);
            DefaultQueryParser dqp = new DefaultQueryParser(null);
            String actualQuery = dqp.getQueryString(solrQuery, null);
            parent = new SimpleQuery("{!parent which=notice_type:notice}" + actualQuery);
        }
        FacetQuery query = new SimpleFacetQuery();
        if (parent.getCriteria() != null) {
            query = new SimpleFacetQuery(parent.getCriteria(), page);
            if (!criteriaNotice.isEmpty()) {
                query.addFilterQuery(new SimpleFilterQuery(buildQuery(criteriaNotice)));
            }
        } else {
            query.addCriteria(buildQuery(criteriaNotice));
        }
        return query;
    }
}
