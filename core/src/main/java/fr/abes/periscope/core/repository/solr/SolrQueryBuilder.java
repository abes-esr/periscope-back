package fr.abes.periscope.core.repository.solr;

import fr.abes.periscope.core.criterion.*;
import fr.abes.periscope.core.exception.IllegalCriterionException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.solr.core.query.Criteria;
import org.springframework.data.solr.core.query.FilterQuery;
import org.springframework.data.solr.core.query.SimpleFilterQuery;

import java.util.Iterator;
import java.util.List;

/**
 * Représente un constructeur de requête SolR pour Periscope
 */
@Slf4j
public class SolrQueryBuilder {

    /**
     * Construit la requête SolR à partir des critères de recherche
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
                try {
                    Criteria pcpQuery = buildPcpQuery((CriterionPcp) criterion);
                    filterQuery.addCriteria(pcpQuery);
                } catch (IllegalCriterionException ex) {
                    log.error(ex.getLocalizedMessage());
                }
            }

            // Bloc de critère RCR
            if (criterion instanceof CriterionRcr) {
                try {
                    Criteria rcrQuery = buildRcrQuery((CriterionRcr) criterion);
                    filterQuery.addCriteria(rcrQuery);
                } catch (IllegalCriterionException ex) {
                    log.error(ex.getLocalizedMessage());
                }
            }

            // Bloc de critère Mots du titre
            if (criterion instanceof CriterionTitleWords) {
                try {
                    Criteria titleWordsQuery = buildTitleWordsQuery((CriterionTitleWords) criterion);
                    filterQuery.addCriteria(titleWordsQuery);
                } catch (IllegalCriterionException ex) {
                    log.error(ex.getLocalizedMessage());
                }
            }

            //Bloc de critère PPN
            if (criterion instanceof CriterionPpn) {
                try {
                    Criteria ppnQuery = buildPpnQuery((CriterionPpn) criterion);
                    filterQuery.addCriteria(ppnQuery);
                } catch (IllegalCriterionException ex) {
                    log.error(ex.getLocalizedMessage());
                }
            }

            //Bloc de critère pays
            if (criterion instanceof CriterionCountry) {
                try {
                    Criteria countryQuery = buildCountryQuery((CriterionCountry) criterion);
                    filterQuery.addCriteria(countryQuery);
                } catch (IllegalCriterionException ex) {
                    log.error(ex.getLocalizedMessage());
                }
            }

            //Bloc de critère code langue
            if (criterion instanceof CriterionLanguage) {
                try {
                    Criteria languageQuery = buildLanguageQuery((CriterionLanguage) criterion);
                    filterQuery.addCriteria(languageQuery);
                } catch (IllegalCriterionException ex) {
                    log.error(ex.getLocalizedMessage());
                }
            }

            //Bloc de critère éditeur
            if (criterion instanceof CriterionEditor) {
                try {
                    Criteria countryQuery = buildEditorQuery((CriterionEditor) criterion);
                    filterQuery.addCriteria(countryQuery);
                } catch (IllegalCriterionException ex) {
                    log.error(ex.getLocalizedMessage());
                }
            }

            // bloc de critère ISSN
            if (criterion instanceof CriterionIssn) {
                try {
                    Criteria issnQuery = buildIssnQuery((CriterionIssn)criterion);
                    filterQuery.addCriteria(issnQuery);
                } catch (IllegalCriterionException ex) {
                    log.error(ex.getLocalizedMessage());
                }
            }
        }

        return filterQuery.getCriteria();
    }

    /**
     * Construit la requête SolR à partir d'un critère de recherche par PCP
     * @param criterion Les critères de recherche par PCP
     * @return Criteria Requête SolR
     * @exception IllegalCriterionException Si la liste des critères est vide
     */
    private Criteria buildPcpQuery(CriterionPcp criterion) throws IllegalCriterionException {

        if (criterion.getPcp().isEmpty()) {
            throw new IllegalCriterionException("Criteria list cannot be empty");
        }

        Iterator<String> pcpIterator = criterion.getPcp().iterator();
        String pcpCode = pcpIterator.next();

        Criteria myCriteria = new Criteria(NoticeField.PCP_S).is(pcpCode);

        while (pcpIterator.hasNext()) {
            pcpCode = pcpIterator.next();
            myCriteria = myCriteria.or(NoticeField.PCP_S).is(pcpCode);
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
     * @param criterion Les critères de recherche par RCR
     * @return Criteria Requête SolR
     * @exception IllegalCriterionException Si la liste des critères est vide
     */
    private Criteria buildRcrQuery(CriterionRcr criterion) throws IllegalCriterionException {

        if (criterion.getRcr().isEmpty()) {
            throw new IllegalCriterionException("Criteria list cannot be empty");
        }

        Iterator<String> rcrIterator = criterion.getRcr().iterator();
        Iterator<String> rcrOperatorIterator = criterion.getRcrOperators().iterator();

        Criteria myCriteria = null;

        String rcrCode = rcrIterator.next();
        String rcrOperator = rcrOperatorIterator.next();

        // 1er critère
        switch (rcrOperator) {
            case LogicalOperator.EXCEPT:
                myCriteria = new Criteria(NoticeField.RCR_S).is(rcrCode).not();
                break;
            default:
                myCriteria = new Criteria(NoticeField.RCR_S).is(rcrCode);
                break;
        }

        // les autres
        while (rcrIterator.hasNext()) {
            rcrCode = rcrIterator.next();
            rcrOperator = rcrOperatorIterator.next();

            switch (rcrOperator) {
                case LogicalOperator.AND:
                    myCriteria = myCriteria.and(NoticeField.RCR_S).is(rcrCode);
                    break;
                case LogicalOperator.OR:
                    myCriteria = myCriteria.or(NoticeField.RCR_S).is(rcrCode);
                    break;
                case LogicalOperator.EXCEPT:
                    myCriteria = myCriteria.and(NoticeField.RCR_S).is(rcrCode).not();
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
     * @param criterion Les critères de recherche par mots du titre
     * @return Criteria Requête SolR
     * @exception IllegalCriterionException Si la liste des critères est vide
     */
    private Criteria buildTitleWordsQuery(CriterionTitleWords criterion) throws IllegalCriterionException {

        if (criterion.getTitleWords().isEmpty()) {
            throw new IllegalCriterionException("Criteria list cannot be empty");
        }

        Iterator<String> valueIterator = criterion.getTitleWords().iterator();
        Iterator<String> operatorIterator = criterion.getTitleWordOperators().iterator();

        Criteria myCriteria = null;

        String value = valueIterator.next();
        String operator = operatorIterator.next();

        // 1er critère
        switch (operator) {
            case LogicalOperator.EXCEPT:
                myCriteria = new Criteria(NoticeField.KEY_TITLE_T).is(value).not().
                        or(NoticeField.KEY_SHORTED_TITLE_T).is(value).not().
                        or(NoticeField.PROPER_TITLE_T).is(value).not().
                        or(NoticeField.TITLE_FROM_DIFFERENT_AUTHOR_T).is(value).not().
                        or(NoticeField.PARALLEL_TITLE_T).is(value).not().
                        or(NoticeField.TITLE_COMPLEMENT_T).is(value).not().
                        or(NoticeField.SECTION_TITLE_T).is(value).not().connect();
                break;
            default:
                myCriteria = new Criteria(NoticeField.KEY_TITLE_T).is(value).
                        or(NoticeField.KEY_SHORTED_TITLE_T).is(value).
                        or(NoticeField.PROPER_TITLE_T).is(value).
                        or(NoticeField.TITLE_FROM_DIFFERENT_AUTHOR_T).is(value).
                        or(NoticeField.PARALLEL_TITLE_T).is(value).
                        or(NoticeField.TITLE_COMPLEMENT_T).is(value).
                        or(NoticeField.SECTION_TITLE_T).is(value).connect();
                break;
        }

        // les autres
        while (valueIterator.hasNext()) {
            value = valueIterator.next();
            operator = operatorIterator.next();

            switch (operator) {
                case LogicalOperator.AND:
                    myCriteria = myCriteria.connect().and(NoticeField.KEY_TITLE_T).is(value).
                            or(NoticeField.KEY_SHORTED_TITLE_T).is(value).
                            or(NoticeField.PROPER_TITLE_T).is(value).
                            or(NoticeField.TITLE_FROM_DIFFERENT_AUTHOR_T).is(value).
                            or(NoticeField.PARALLEL_TITLE_T).is(value).
                            or(NoticeField.TITLE_COMPLEMENT_T).is(value).
                            or(NoticeField.SECTION_TITLE_T).is(value);
                    break;
                case LogicalOperator.OR:
                    myCriteria = myCriteria.connect().or(NoticeField.KEY_TITLE_T).is(value).
                            or(NoticeField.KEY_SHORTED_TITLE_T).is(value).
                            or(NoticeField.PROPER_TITLE_T).is(value).
                            or(NoticeField.TITLE_FROM_DIFFERENT_AUTHOR_T).is(value).
                            or(NoticeField.PARALLEL_TITLE_T).is(value).
                            or(NoticeField.TITLE_COMPLEMENT_T).is(value).
                            or(NoticeField.SECTION_TITLE_T).is(value);
                    break;
                case LogicalOperator.EXCEPT:
                    myCriteria = myCriteria.connect().and(NoticeField.KEY_TITLE_T).is(value).not().
                            or(NoticeField.KEY_SHORTED_TITLE_T).is(value).not().
                            or(NoticeField.PROPER_TITLE_T).is(value).not().
                            or(NoticeField.TITLE_FROM_DIFFERENT_AUTHOR_T).is(value).not().
                            or(NoticeField.PARALLEL_TITLE_T).is(value).not().
                            or(NoticeField.TITLE_COMPLEMENT_T).is(value).not().
                            or(NoticeField.SECTION_TITLE_T).is(value).not();
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
     * @param criterion Les critères de recherche par code pays
     * @return Criteria Requête SolR
     * @exception IllegalCriterionException Si la liste des critères est vide
     */
    private Criteria buildCountryQuery(CriterionCountry criterion) throws IllegalCriterionException {

        if (criterion.getCountries().isEmpty()) {
            throw new IllegalCriterionException("Criteria list cannot be empty");
        }

        Iterator<String> valueIterator = criterion.getCountries().iterator();
        Iterator<String> operatorIterator = criterion.getCountryOperators().iterator();

        Criteria myCriteria = null;

        String value = valueIterator.next();
        String operator = operatorIterator.next();

        // 1er critère
        switch (operator) {
            case LogicalOperator.EXCEPT:
                myCriteria = new Criteria(NoticeField.COUNTRY_T).is(value).not().connect().and(NoticeField.KEY_TITLE_T);
                break;
            default:
                myCriteria = new Criteria(NoticeField.COUNTRY_T).is(value).connect().and(NoticeField.KEY_TITLE_T);
                break;
        }

        // les autres
        while (valueIterator.hasNext()) {
            value = valueIterator.next();
            operator = operatorIterator.next();

            switch (operator) {
                case LogicalOperator.AND:
                    myCriteria = myCriteria.connect().and(NoticeField.COUNTRY_T).is(value).and(NoticeField.KEY_TITLE_T);
                    break;
                case LogicalOperator.OR:
                    myCriteria = myCriteria.connect().or(NoticeField.COUNTRY_T).is(value).and(NoticeField.KEY_TITLE_T);
                    break;
                case LogicalOperator.EXCEPT:
                    myCriteria = myCriteria.connect().and(NoticeField.COUNTRY_T).is(value).not().and(NoticeField.KEY_TITLE_T);
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
     * @param criterion Les critères de recherche par PPN
     * @return Criteria Requête SolR
     * @exception IllegalCriterionException Si la liste des critères est vide
     */
    private Criteria buildPpnQuery(CriterionPpn criterion) throws IllegalCriterionException {

        if (criterion.getPpn().isEmpty()) {
            throw new IllegalCriterionException("Criteria list cannot be empty");
        }

        Iterator<String> valueIterator = criterion.getPpn().iterator();

        Criteria myCriteria;

        String value = valueIterator.next();

        myCriteria = new Criteria(NoticeField.PPN).is(value);

        // les autres
        while (valueIterator.hasNext()) {
            value = valueIterator.next();
            myCriteria = myCriteria.or(NoticeField.PPN).is(value);
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
     * @param criterion Les critères de recherche par code langue
     * @return Criteria Requête SolR
     */
    private Criteria buildLanguageQuery(CriterionLanguage criterion) {

        if (criterion.getLanguages().isEmpty()) {
            throw new IllegalCriterionException("Criteria list cannot be empty");
        }

        Iterator<String> langueIterator = criterion.getLanguages().iterator();
        Iterator<String> langueOperatorIterator = criterion.getLanguageOperators().iterator();

        Criteria myCriteria = null;

        String langueCode = langueIterator.next();
        String langueOperator = langueOperatorIterator.next();

        // 1er critère
        switch (langueOperator) {
            case LogicalOperator.EXCEPT:
                myCriteria = new Criteria(NoticeField.LANGUAGE).is(langueCode).not().connect();
                break;
            default:
                myCriteria = new Criteria(NoticeField.LANGUAGE).is(langueCode).connect();
                break;
        }

        // les autres
        while (langueIterator.hasNext()) {
            langueCode = langueIterator.next();
            langueOperator = langueOperatorIterator.next();

            switch (langueOperator) {
                case LogicalOperator.AND:
                    myCriteria = myCriteria.connect().and(NoticeField.LANGUAGE).is(langueCode);
                    break;
                case LogicalOperator.OR:
                    myCriteria = myCriteria.connect().or(NoticeField.LANGUAGE).is(langueCode);
                    break;
                case LogicalOperator.EXCEPT:
                    myCriteria = myCriteria.connect().and(NoticeField.LANGUAGE).is(langueCode).not();
                    break;
            }
        }
        myCriteria = myCriteria.and(NoticeField.KEY_TITLE_T);

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
     * @param criterion Les critères de recherche par editeur
     * @return Criteria Requête SolR
     * @exception IllegalCriterionException Si la liste des critères est vide
     */
    private Criteria buildEditorQuery(CriterionEditor criterion) throws IllegalCriterionException {

        if (criterion.getEditors().isEmpty()) {
            throw new IllegalCriterionException("Criteria list cannot be empty");
        }

        Iterator<String> rcrIterator = criterion.getEditors().iterator();
        Iterator<String> rcrOperatorIterator = criterion.getEditorOperators().iterator();

        Criteria myCriteria = null;

        String rcrCode = rcrIterator.next();
        String rcrOperator = rcrOperatorIterator.next();

        // 1er critère
        switch (rcrOperator) {
            case LogicalOperator.EXCEPT:
                myCriteria = new Criteria(NoticeField.EDITOR_T).is(rcrCode).not().connect();
                break;
            default:
                myCriteria = new Criteria(NoticeField.EDITOR_T).is(rcrCode).connect();
                break;
        }

        // les autres
        while (rcrIterator.hasNext()) {
            rcrCode = rcrIterator.next();
            rcrOperator = rcrOperatorIterator.next();

            switch (rcrOperator) {
                case LogicalOperator.AND:
                    myCriteria = myCriteria.connect().and(NoticeField.EDITOR_T).is(rcrCode);
                    break;
                case LogicalOperator.OR:
                    myCriteria = myCriteria.connect().or(NoticeField.EDITOR_T).is(rcrCode);
                    break;
                case LogicalOperator.EXCEPT:
                    myCriteria = myCriteria.connect().and(NoticeField.EDITOR_T).is(rcrCode).not();
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

     /* Construit la requête SolR à partir d'un critère de recherche par ISSN
     * @param critetion Les critères de recherche par ISSN
     * @return Criteria Requête SolR
     */
    private Criteria buildIssnQuery(CriterionIssn criterion) {

        if (criterion.getIssn().isEmpty()) {
            throw new IllegalCriterionException("Criteria list cannot be empty");
        }

        Iterator<String> issnIterator = criterion.getIssn().iterator();

        Criteria myCriteria;

        String value = issnIterator.next();
        myCriteria = new Criteria(NoticeField.ISSN_T).is(value);

        // les autres
        while (issnIterator.hasNext()) {
            value = issnIterator.next();
            myCriteria = myCriteria.or(NoticeField.ISSN_T).is(value);
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
}
