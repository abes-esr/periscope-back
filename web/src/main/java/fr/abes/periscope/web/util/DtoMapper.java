package fr.abes.periscope.web.util;

import fr.abes.periscope.core.criterion.*;
import fr.abes.periscope.core.entity.solr.*;
import fr.abes.periscope.core.entity.visualisation.NoticeVisu;
import fr.abes.periscope.core.entity.visualisation.SequenceContinue;
import fr.abes.periscope.core.entity.visualisation.SequenceError;
import fr.abes.periscope.core.entity.visualisation.SequenceLacune;
import fr.abes.periscope.core.exception.*;
import fr.abes.periscope.core.util.UtilsMapper;
import fr.abes.periscope.web.dto.*;
import fr.abes.periscope.web.dto.criterion.*;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.Converter;
import org.modelmapper.spi.MappingContext;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.stereotype.Service;

import java.util.*;

/**
 * Utilitaire de mapping de objets Entité (core) et des objets DTO (API)
 */
@Service
@Slf4j
public class DtoMapper {
    @Value("${url.sudoc}")
    private String SUDOC_URL;

    private UtilsMapper utilsMapper;

    @Autowired
    public DtoMapper(UtilsMapper utilsMapper) { this.utilsMapper = utilsMapper; }

    @Bean
    public void converterResultWebDto() {
        Converter<ResultSolr, ResultWebDto> myConverter = new Converter<ResultSolr, ResultWebDto>() {
            @Override
            public ResultWebDto convert(MappingContext<ResultSolr, ResultWebDto> mappingContext) {
                ResultSolr resultSolr = mappingContext.getSource();
                ResultWebDto resultWebDto = new ResultWebDto();
                resultWebDto.setNbPages(resultSolr.getNbPages());
                resultWebDto.setNbNotices(resultSolr.getNbNotices());
                resultWebDto.setNotices(utilsMapper.mapList(resultSolr.getNotices(), NoticeWebV2Dto.class));
                resultSolr.getFacettes().forEach(f -> {
                    FacetteWebDto facetteWebDto = new FacetteWebDto();
                    Arrays.stream(NoticeSolrField.class.getFields()).forEach(field -> {
                        try {
                            if (field.get(null).equals(f.getZone())) {
                                facetteWebDto.setZone(field.getName());
                            }
                        } catch (IllegalAccessException e) {
                            log.error("Impossible de récupérer la facette " + f.getZone());
                        }
                    });
                    Iterator<Map<String, Integer>> itValeurs = f.getValeurs().iterator();
                    while (itValeurs.hasNext()) {
                        Map<String, Integer> val = itValeurs.next();
                        FacetteContentWebDto mapCleValeur = new FacetteContentWebDto();
                        Iterator<String> itKey = val.keySet().iterator();
                        while (itKey.hasNext()) {
                            String key = itKey.next();
                            mapCleValeur.setKey(key);
                            mapCleValeur.setOccurrence(val.get(key));
                        }
                        facetteWebDto.addValeur(mapCleValeur);
                    }
                    resultWebDto.addFacette(facetteWebDto);
                });

                return resultWebDto;
            }
        };
        utilsMapper.addConverter(myConverter);
    }

    @Bean
    public void converterNotice() {
        Converter<Notice, NoticeWebV2Dto> myConverter = new Converter<Notice, NoticeWebV2Dto>() {
            @Override
            public NoticeWebV2Dto convert(MappingContext<Notice, NoticeWebV2Dto> mappingContext) {
                Notice notice = mappingContext.getSource();
                NoticeWebV2Dto noticeWeb = new NoticeWebV2Dto();
                noticeWeb.setPpn(notice.getPpn());
                noticeWeb.setIssn(notice.getIssn());
                noticeWeb.setEditeur(notice.getPublisher());
                noticeWeb.setTitre(utilsMapper.getTitre(notice.getKeyTitle(), notice.getKeyTitleQualifer(), notice.getKeyShortedTitle(), notice.getProperTitle(), notice.getTitleFromDifferentAuthor(), notice.getParallelTitle(), notice.getTitleComplement()));
                noticeWeb.setTypeRessourceContinue(notice.getContinuousType());
                noticeWeb.setStartYear(notice.getStartYear());
                noticeWeb.setEndYear(notice.getEndYear());
                noticeWeb.setNbLocation(notice.getNbLocation());
                noticeWeb.setPcpList(notice.getPcpList());

                if (notice.getNbLocation() != 0)
                    noticeWeb.setSudocURL(SUDOC_URL + notice.getPpn());

                notice.getItems().forEach(i -> noticeWeb.addRcr(i.getRcr()));
                Collections.sort(noticeWeb.getRcrList());
                return noticeWeb;
            }
        };
        utilsMapper.addConverter(myConverter);
    }

    /**
     * Convertisseur pour les critères de tri
     * On vérifie la definition des champs depuis la classe NoticeV1SolrField ou NoticeV2SolrField en fonction de la version
     */
    @Bean
    public void converterSort() {
        Converter<CriterionSortWebDto, CriterionSort> myConverter = new Converter<CriterionSortWebDto, CriterionSort>() {
            @Override
            public CriterionSort convert(MappingContext<CriterionSortWebDto, CriterionSort> mappingContext) {
                CriterionSortWebDto s = mappingContext.getSource();
                final String[] field = {""};
                try {
                    field[0] = getSortField(s);
                    if (field[0].isEmpty()) {
                        throw new IllegalSortException(s.getSort() + " : Critère de tri inconnu");
                    }
                    CriterionSort d = new CriterionSort(field[0], s.getOrder());
                    return d;
                } catch (IllegalSortException ex) {
                    throw new IllegalSortException(s.getSort() + " : " + ex.getMessage());
                }
            }
        };
        utilsMapper.addConverter(myConverter);
    }


    /**
     * Méthode de récupération d'un critère de tri pour la v2 de périscope
     *
     * @param s critère de tri
     * @return valeur dans NoticeV2SolrField correspondant au critère saisi
     */
    private String getSortField(CriterionSortWebDto s) {
        switch (s.getSort().toLowerCase(Locale.ROOT)) {
            case "title_type":
                return NoticeSolrField.TITLE_TYPE;
            case "ppn":
                return NoticeSolrField.PPN;
            case "issn":
                return NoticeSolrField.ISSN;
            case "language":
                return NoticeSolrField.LANGUAGE;
            case "country":
                return NoticeSolrField.COUNTRY;
            case "document_type":
                return NoticeSolrField.DOCUMENT_TYPE;
            case "support_type":
                return NoticeSolrField.SUPPORT_TYPE;
            case "editor":
                return NoticeSolrField.EDITOR_Z;
            case "title":
                return NoticeSolrField.TRI_TITRE;
            case "start_year":
                return NoticeSolrField.START_YEAR;
            case "end_year":
                return NoticeSolrField.END_YEAR;
            case "nb_loc":
                return NoticeSolrField.NB_LOC;
            case "pcp_list":
                return NoticeSolrField.NB_PCP;
            default:
                throw new IllegalSortException(s.getSort() + " : Critère de tri inconnu");
        }
    }


    /**
     * Convertisseur pour les facettes (DTO vers Objet Métier)
     */
    @Bean
    public void converterFacette() {
        Converter<CriterionFacetteWebDto, String> myConverter = new Converter<CriterionFacetteWebDto, String>() {
            @Override
            public String convert(MappingContext<CriterionFacetteWebDto, String> context) {
                CriterionFacetteWebDto s = context.getSource();
                if (!Arrays.stream(NoticeSolrField.class.getDeclaredFields()).anyMatch(f -> f.getName().toLowerCase(Locale.ROOT).equals(s.getZone().toLowerCase(Locale.ROOT)))
                        && (!Arrays.stream(ItemSolrField.class.getDeclaredFields()).anyMatch(f -> f.getName().toLowerCase(Locale.ROOT).equals(s.getZone().toLowerCase(Locale.ROOT))))) {
                    throw new IllegalFacetteException("Facette : " + s.getZone() + " non disponible dans le schéma d'indexation");
                }
                return s.getZone();
            }
        };
        utilsMapper.addConverter(myConverter);
    }

    /**
     * Convertisseur pour les critères PCP (DTO vers objet métier)
     */
    @Bean
    public void converterPcp() {

        Converter<CriterionPcpWebDto, CriterionPcp> myConverter = new Converter<CriterionPcpWebDto, CriterionPcp>() {

            public CriterionPcp convert(MappingContext<CriterionPcpWebDto, CriterionPcp> context) {
                CriterionPcpWebDto s = context.getSource();
                try {
                    if (s.getBlocOperator() == null) {
                        throw new IllegalArgumentException(CriterionTypeName.CRITERION_PCP + " - property '" + CriterionWebDto.OPERATOR_PROPERTY + "' is missing");
                    }
                    if (s.getPcp() == null) {
                        throw new IllegalArgumentException(CriterionTypeName.CRITERION_PCP + " - property '" + CriterionPcpWebDto.PCP_PROPERTY + "' is missing");
                    }
                    if (s.getPcpOperator() == null) {
                        throw new IllegalArgumentException(CriterionTypeName.CRITERION_PCP + " - property '" + CriterionPcpWebDto.PCP_OPERATOR_PROPERTY + "' is missing");
                    }

                    CriterionPcp d = new CriterionPcp(s.getBlocOperator(), s.getPcp(), s.getPcpOperator());
                    return d;

                } catch (IllegalOperatorException ex) {
                    throw new IllegalOperatorException(CriterionTypeName.CRITERION_PCP + " : " + ex.getLocalizedMessage());
                } catch (CriterionOperatorMismatchException ex) {
                    throw new CriterionOperatorMismatchException(CriterionTypeName.CRITERION_PCP + " - '" + CriterionPcpWebDto.PCP_PROPERTY + "' dismatch size of '" + CriterionPcpWebDto.PCP_OPERATOR_PROPERTY + "' : " + ex.getLocalizedMessage());
                } catch (IllegalCriterionException ex) {
                    throw new IllegalCriterionException(CriterionTypeName.CRITERION_PCP + " : " + ex.getLocalizedMessage());
                }
            }
        };
        utilsMapper.addConverter(myConverter);
    }

    /**
     * Convertisseur pour les critères RCR (DTO vers objet métier)
     */
    @Bean
    public void converterRcr() {

        Converter<CriterionRcrWebDto, CriterionRcr> myConverter = new Converter<CriterionRcrWebDto, CriterionRcr>() {
            public CriterionRcr convert(MappingContext<CriterionRcrWebDto, CriterionRcr> context) {
                CriterionRcrWebDto s = context.getSource();
                try {
                    if (s.getBlocOperator() == null) {
                        throw new IllegalArgumentException(CriterionTypeName.CRITERION_RCR + " - property '" + CriterionWebDto.OPERATOR_PROPERTY + "' is missing");
                    }
                    if (s.getRcr() == null) {
                        throw new IllegalArgumentException(CriterionTypeName.CRITERION_RCR + " - property '" + CriterionRcrWebDto.RCR_PROPERTY + "' is missing");
                    }
                    if (s.getRcrOperator() == null) {
                        throw new IllegalArgumentException(CriterionTypeName.CRITERION_RCR + " - property '" + CriterionRcrWebDto.RCR_OPERATOR_PROPERTY + "' is missing");
                    }

                    CriterionRcr d = new CriterionRcr(s.getBlocOperator(), new ArrayList<>(s.getRcr()), s.getRcrOperator());
                    return d;
                } catch (IllegalOperatorException ex) {
                    throw new IllegalOperatorException(CriterionTypeName.CRITERION_RCR + " : " + ex.getLocalizedMessage());
                } catch (CriterionOperatorMismatchException ex) {
                    throw new CriterionOperatorMismatchException(CriterionTypeName.CRITERION_RCR + " - '" + CriterionRcrWebDto.RCR_PROPERTY + "' dismatch size of '" + CriterionRcrWebDto.RCR_OPERATOR_PROPERTY + "' : " + ex.getLocalizedMessage());
                } catch (IllegalCriterionException ex) {
                    throw new IllegalCriterionException(CriterionTypeName.CRITERION_RCR + " : " + ex.getLocalizedMessage());
                }
            }
        };
        utilsMapper.addConverter(myConverter);
    }

    /**
     * Convertisseur pour les critères PPN (DTO vers objet métier)
     */
    @Bean
    public void converterPpn() {

        Converter<CriterionPpnWebDto, CriterionPpn> myConverter = new Converter<CriterionPpnWebDto, CriterionPpn>() {
            public CriterionPpn convert(MappingContext<CriterionPpnWebDto, CriterionPpn> context) {
                CriterionPpnWebDto s = context.getSource();
                try {
                    if (s.getBlocOperator() == null) {
                        throw new IllegalArgumentException(CriterionTypeName.CRITERION_PPN + " - property '" + CriterionWebDto.OPERATOR_PROPERTY + "' is missing");
                    }
                    if (s.getPpn() == null) {
                        throw new IllegalArgumentException(CriterionTypeName.CRITERION_PPN + " - property '" + CriterionPpnWebDto.PPN_PROPERTY + "' is missing");
                    }

                    CriterionPpn d = new CriterionPpn(s.getBlocOperator(), s.getPpn());
                    return d;
                } catch (IllegalOperatorException ex) {
                    throw new IllegalOperatorException(CriterionTypeName.CRITERION_PPN + " : " + ex.getLocalizedMessage());
                } catch (CriterionOperatorMismatchException ex) {
                    throw new CriterionOperatorMismatchException(CriterionTypeName.CRITERION_PPN + " - '" + CriterionPpnWebDto.PPN_PROPERTY + "' dismatch size of '" + CriterionPpnWebDto.PPN_OPERATOR_PROPERTY + "' : " + ex.getLocalizedMessage());
                } catch (IllegalCriterionException ex) {
                    throw new IllegalCriterionException(CriterionTypeName.CRITERION_PPN + " : " + ex.getLocalizedMessage());
                }
            }
        };
        utilsMapper.addConverter(myConverter);
    }

    /**
     * Convertisseur pour les critères Mots du titre (DTO vers objet métier)
     */
    @Bean
    public void converterTitleWords() {

        Converter<CriterionTitleWordsWebDto, CriterionTitleWords> myConverter = new Converter<CriterionTitleWordsWebDto, CriterionTitleWords>() {
            public CriterionTitleWords convert(MappingContext<CriterionTitleWordsWebDto, CriterionTitleWords> context) {
                CriterionTitleWordsWebDto s = context.getSource();
                try {

                    if (s.getBlocOperator() == null) {
                        throw new IllegalArgumentException(CriterionTypeName.CRITERION_TITLE_WORDS + " - property '" + CriterionWebDto.OPERATOR_PROPERTY + "' is missing");
                    }
                    if (s.getTitleWords() == null) {
                        throw new IllegalArgumentException(CriterionTypeName.CRITERION_TITLE_WORDS + " - property '" + CriterionTitleWordsWebDto.TITLE_WORDS_PROPERTY + "' is missing");
                    }
                    if (s.getTitleWordsOperator() == null) {
                        throw new IllegalArgumentException(CriterionTypeName.CRITERION_TITLE_WORDS + " - property '" + CriterionTitleWordsWebDto.TITLE_WORDS_OPERATOR_PROPERTY + "' is missing");
                    }

                    CriterionTitleWords d = new CriterionTitleWords(s.getBlocOperator(), s.getTitleWords(), s.getTitleWordsOperator());
                    return d;
                } catch (IllegalOperatorException ex) {
                    throw new IllegalOperatorException(CriterionTypeName.CRITERION_TITLE_WORDS + " : " + ex.getLocalizedMessage());
                } catch (CriterionOperatorMismatchException ex) {
                    throw new CriterionOperatorMismatchException(CriterionTypeName.CRITERION_TITLE_WORDS + " - '" + CriterionTitleWordsWebDto.TITLE_WORDS_PROPERTY + "' dismatch size of '" + CriterionTitleWordsWebDto.TITLE_WORDS_OPERATOR_PROPERTY + "' : " + ex.getLocalizedMessage());
                } catch (IllegalCriterionException ex) {
                    throw new IllegalCriterionException(CriterionTypeName.CRITERION_TITLE_WORDS + " : " + ex.getLocalizedMessage());
                }
            }
        };
        utilsMapper.addConverter(myConverter);
    }

    /**
     * Convertisseur pour les critères ISSN (DTO vers objet métier)
     */
    @Bean
    public void converterIssn() {

        Converter<CriterionIssnWebDto, CriterionIssn> myConverter = new Converter<CriterionIssnWebDto, CriterionIssn>() {
            @Override
            public CriterionIssn convert(MappingContext<CriterionIssnWebDto, CriterionIssn> mappingContext) {
                CriterionIssnWebDto s = mappingContext.getSource();
                try {

                    if (s.getBlocOperator() == null) {
                        throw new IllegalArgumentException(CriterionTypeName.CRITERION_ISSN + " - property '" + CriterionWebDto.OPERATOR_PROPERTY + "' is missing");
                    }
                    if (s.getIssn() == null) {
                        throw new IllegalArgumentException(CriterionTypeName.CRITERION_ISSN + " - property '" + CriterionIssnWebDto.ISSN_PROPERTY + "' is missing");
                    }

                    CriterionIssn d = new CriterionIssn(s.getBlocOperator(), s.getIssn());
                    return d;
                } catch (IllegalOperatorException ex) {
                    throw new IllegalOperatorException(CriterionTypeName.CRITERION_ISSN + " : " + ex.getLocalizedMessage());
                } catch (CriterionOperatorMismatchException ex) {
                    throw new CriterionOperatorMismatchException(CriterionTypeName.CRITERION_ISSN + " - '" + CriterionIssnWebDto.ISSN_PROPERTY + "' dismatch size of '" + CriterionIssnWebDto.ISSN_OPERATOR_PROPERTY + "' : " + ex.getLocalizedMessage());
                } catch (IllegalCriterionException ex) {
                    throw new IllegalCriterionException(CriterionTypeName.CRITERION_ISSN + " : " + ex.getLocalizedMessage());
                }
            }
        };
        utilsMapper.addConverter(myConverter);
    }

    /**
     * Convertisseur pour les critères éditeurs (DTO vers objet métier)
     */
    @Bean
    public void converterEditors() {

        Converter<CriterionEditorWebDto, CriterionEditor> myConverter = new Converter<CriterionEditorWebDto, CriterionEditor>() {
            public CriterionEditor convert(MappingContext<CriterionEditorWebDto, CriterionEditor> context) {
                CriterionEditorWebDto s = context.getSource();
                try {

                    if (s.getBlocOperator() == null) {
                        throw new IllegalArgumentException(CriterionTypeName.CRITERION_EDITOR + " - property '" + CriterionWebDto.OPERATOR_PROPERTY + "' is missing");
                    }
                    if (s.getEditors() == null) {
                        throw new IllegalArgumentException(CriterionTypeName.CRITERION_EDITOR + " - property '" + CriterionEditorWebDto.EDITORS_PROPERTY + "' is missing");
                    }
                    if (s.getEditorsOperator() == null) {
                        throw new IllegalArgumentException(CriterionTypeName.CRITERION_EDITOR + " - property '" + CriterionEditorWebDto.EDITORS_OPERATOR_PROPERTY + "' is missing");
                    }

                    CriterionEditor d = new CriterionEditor(s.getBlocOperator(), s.getEditors(), s.getEditorsOperator());
                    return d;
                } catch (IllegalOperatorException ex) {
                    throw new IllegalOperatorException(CriterionTypeName.CRITERION_EDITOR + " : " + ex.getLocalizedMessage());
                } catch (CriterionOperatorMismatchException ex) {
                    throw new CriterionOperatorMismatchException(CriterionTypeName.CRITERION_EDITOR + " - '" + CriterionEditorWebDto.EDITORS_PROPERTY + "' dismatch size of '" + CriterionEditorWebDto.EDITORS_OPERATOR_PROPERTY + "' : " + ex.getLocalizedMessage());
                } catch (IllegalCriterionException ex) {
                    throw new IllegalCriterionException(CriterionTypeName.CRITERION_EDITOR + " : " + ex.getLocalizedMessage());
                }
            }
        };
        utilsMapper.addConverter(myConverter);
    }

    /**
     * Convertisseur pour les critères pays (DTO vers objet métier)
     */
    @Bean
    public void converterCountry() {

        Converter<CriterionCountryWebDto, CriterionCountry> myConverter = new Converter<CriterionCountryWebDto, CriterionCountry>() {
            public CriterionCountry convert(MappingContext<CriterionCountryWebDto, CriterionCountry> context) {
                CriterionCountryWebDto s = context.getSource();
                try {

                    if (s.getBlocOperator() == null) {
                        throw new IllegalArgumentException(CriterionTypeName.CRITERION_COUNTRIES + " - property '" + CriterionWebDto.OPERATOR_PROPERTY + "' is missing");
                    }
                    if (s.getCountries() == null) {
                        throw new IllegalArgumentException(CriterionTypeName.CRITERION_COUNTRIES + " - property '" + CriterionCountryWebDto.COUNTRIES_PROPERTY + "' is missing");
                    }
                    if (s.getCountriesOperator() == null) {
                        throw new IllegalArgumentException(CriterionTypeName.CRITERION_COUNTRIES + " - property '" + CriterionCountryWebDto.COUNTRIES_OPERATOR_PROPERTY + "' is missing");
                    }

                    CriterionCountry d = new CriterionCountry(s.getBlocOperator(), s.getCountries(), s.getCountriesOperator());
                    return d;
                } catch (IllegalOperatorException ex) {
                    throw new IllegalOperatorException(CriterionTypeName.CRITERION_COUNTRIES + " : " + ex.getLocalizedMessage());
                } catch (CriterionOperatorMismatchException ex) {
                    throw new CriterionOperatorMismatchException(CriterionTypeName.CRITERION_COUNTRIES + " - '" + CriterionCountryWebDto.COUNTRIES_PROPERTY + "' dismatch size of '" + CriterionCountryWebDto.COUNTRIES_OPERATOR_PROPERTY + "' : " + ex.getLocalizedMessage());
                } catch (IllegalCriterionException ex) {
                    throw new IllegalCriterionException(CriterionTypeName.CRITERION_COUNTRIES + " : " + ex.getLocalizedMessage());
                }
            }
        };
        utilsMapper.addConverter(myConverter);
    }

    /**
     * Convertisseur pour les critères Langue (DTO vers objet métier)
     */
    @Bean
    public void converterLangue() {
        Converter<CriterionLanguageWebDto, CriterionLanguage> myConverter = new Converter<CriterionLanguageWebDto, CriterionLanguage>() {
            public CriterionLanguage convert(MappingContext<CriterionLanguageWebDto, CriterionLanguage> context) {
                CriterionLanguageWebDto s = context.getSource();
                try {

                    if (s.getBlocOperator() == null) {
                        throw new IllegalArgumentException(CriterionTypeName.CRITERION_LANGUAGE + " - property '" + CriterionWebDto.OPERATOR_PROPERTY + "' is missing");
                    }
                    if (s.getLanguage() == null) {
                        throw new IllegalArgumentException(CriterionTypeName.CRITERION_LANGUAGE + " - property '" + CriterionLanguageWebDto.LANGUAGE_PROPERTY + "' is missing");
                    }
                    if (s.getLanguageOperators() == null) {
                        throw new IllegalArgumentException(CriterionTypeName.CRITERION_LANGUAGE + " - property '" + CriterionLanguageWebDto.LANGUAGE_OPERATOR_PROPERTY + "' is missing");
                    }

                    CriterionLanguage d = new CriterionLanguage(s.getBlocOperator(), s.getLanguage(), s.getLanguageOperators());
                    return d;
                } catch (IllegalOperatorException ex) {
                    throw new IllegalOperatorException(CriterionTypeName.CRITERION_LANGUAGE + " : " + ex.getLocalizedMessage());
                } catch (CriterionOperatorMismatchException ex) {
                    throw new CriterionOperatorMismatchException(CriterionTypeName.CRITERION_LANGUAGE + " - '" + CriterionLanguageWebDto.LANGUAGE_PROPERTY + "' dismatch size of '" + CriterionLanguageWebDto.LANGUAGE_OPERATOR_PROPERTY + "' : " + ex.getLocalizedMessage());
                } catch (IllegalCriterionException ex) {
                    throw new IllegalCriterionException(CriterionTypeName.CRITERION_LANGUAGE + " : " + ex.getLocalizedMessage());
                }
            }
        };
        utilsMapper.addConverter(myConverter);
    }

    /**
     * Convertisseur pour les critères statut de bibliothèque (DTO vers objet métier)
     */
    @Bean
    public void converterStatutBib() {
        Converter<CriterionStatutBibWebDto, CriterionStatutBib> myConverter = new Converter<CriterionStatutBibWebDto, CriterionStatutBib>() {
            public CriterionStatutBib convert(MappingContext<CriterionStatutBibWebDto, CriterionStatutBib> context) {
                CriterionStatutBibWebDto s = context.getSource();
                try {

                    if (s.getBlocOperator() == null) {
                        throw new IllegalArgumentException(CriterionTypeName.CRITERION_STATUT_BIB + " - property '" + CriterionWebDto.OPERATOR_PROPERTY + "' is missing");
                    }
                    if (s.getStatut() == null) {
                        throw new IllegalArgumentException(CriterionTypeName.CRITERION_STATUT_BIB + " - property '" + CriterionStatutBibWebDto.STATUT_PROPERTY + "' is missing");
                    }
                    if (s.getStatutsOperators() == null) {
                        throw new IllegalArgumentException(CriterionTypeName.CRITERION_STATUT_BIB + " - property '" + CriterionStatutBibWebDto.STATUT_OPERATOR_PROPERTY + "' is missing");
                    }

                    CriterionStatutBib d = new CriterionStatutBib(s.getBlocOperator(), s.getStatut(), s.getStatutsOperators());
                    return d;
                } catch (IllegalOperatorException ex) {
                    throw new IllegalOperatorException(CriterionTypeName.CRITERION_STATUT_BIB + " : " + ex.getLocalizedMessage());
                } catch (CriterionOperatorMismatchException ex) {
                    throw new CriterionOperatorMismatchException(CriterionTypeName.CRITERION_STATUT_BIB + " - '" + CriterionStatutBibWebDto.STATUT_PROPERTY + "' dismatch size of '" + CriterionStatutBibWebDto.STATUT_OPERATOR_PROPERTY + "' : " + ex.getLocalizedMessage());
                } catch (IllegalCriterionException ex) {
                    throw new IllegalCriterionException(CriterionTypeName.CRITERION_STATUT_BIB + " : " + ex.getLocalizedMessage());
                }
            }
        };
        utilsMapper.addConverter(myConverter);
    }

    @Bean
    public void converterNoticeVisuWebDto() {
        Converter<NoticeVisu, NoticeVisuWebDto> myConverter = new Converter<NoticeVisu, NoticeVisuWebDto>() {
            @SneakyThrows
            @Override
            public NoticeVisuWebDto convert(MappingContext<NoticeVisu, NoticeVisuWebDto> context) {
                NoticeVisu notice = context.getSource();
                int endDate = (notice.getEndYear().getYear() == null) ? Calendar.getInstance().get(Calendar.YEAR) : Integer.parseInt(notice.getEndYear().getYear());
                NoticeVisuWebDto noticeVisuWebDto = new NoticeVisuWebDto(Integer.parseInt(notice.getStartYear().getYear()), endDate);
                notice.getHoldings().forEach(h -> {
                    HoldingWebDto holding = new HoldingWebDto();
                    StringBuilder etatCollection = new StringBuilder();
                    if (h.getTextEtatCollection() != null) etatCollection.append(h.getTextEtatCollection());
                    if (h.getMentionDeLacune() != null) etatCollection.append(h.getMentionDeLacune());
                    if (h.getTextLacune() != null) etatCollection.append("[ lacunes signalées : ").append(h.getTextLacune()).append(" ]");
                    holding.setEtatCollectionTextuel(etatCollection.toString());
                    holding.addErreurs(h.getErreurs());
                    h.getAllNonEmptySequences().forEach(s -> {
                        SequenceWebDto sequenceWebDto = new SequenceWebDto(s.getStartDate(), s.getEndDate(), h.getRcr());
                        if (s instanceof SequenceContinue) sequenceWebDto.setTypeSequence(TYPE_SEQUENCE.CONTINUE);
                        else if (s instanceof SequenceError) {
                            sequenceWebDto.setTypeSequence(TYPE_SEQUENCE.ERREUR);
                            holding.addErreur(((SequenceError) s).getMessage());
                        } else if (s instanceof SequenceLacune) sequenceWebDto.setTypeSequence(TYPE_SEQUENCE.LACUNE);
                        holding.addSequence(sequenceWebDto);
                    });
                    noticeVisuWebDto.addHolding(holding);
                });
                noticeVisuWebDto.addHoldingAgregee();
                return noticeVisuWebDto;
            }
        };
        utilsMapper.addConverter(myConverter);
    }

    @Bean
    public void converterNoticeInfoWebDto() {
        Converter<NoticeVisu, NoticeInfoWebDto> myConverter = new Converter<NoticeVisu, NoticeInfoWebDto>() {
            @SneakyThrows
            @Override
            public NoticeInfoWebDto convert(MappingContext<NoticeVisu, NoticeInfoWebDto> context) {
                NoticeVisu notice = context.getSource();
                NoticeInfoWebDto noticeInfoWebDto = new NoticeInfoWebDto();
                noticeInfoWebDto.setPpn(notice.getPpn());
                noticeInfoWebDto.setIssn(notice.getIssn());
                noticeInfoWebDto.setEditeur(notice.getPublisher());
                noticeInfoWebDto.setTypeSupport(notice.getSupportType());
                noticeInfoWebDto.setTitre(utilsMapper.getTitre(notice.getKeyTitle(), notice.getKeyTitleQualifer(), notice.getKeyShortedTitle(), notice.getProperTitle(), notice.getTitleFromDifferentAuthor(), notice.getParallelTitle(), notice.getTitleComplement()));
                if(notice.getEndYear().getYear() != null) {
                    noticeInfoWebDto.setDatePublication("(" + notice.getStartYear().getYear() + ")-(" + notice.getEndYear().getYear() + ")");
                }else{
                    noticeInfoWebDto.setDatePublication("(" + notice.getStartYear().getYear() + ")-...");
                }
                noticeInfoWebDto.setPeriodicite(notice.getFrequency());
                noticeInfoWebDto.setVille(notice.getCity());

                return noticeInfoWebDto;
            }
        };
        utilsMapper.addConverter(myConverter);
    }
}
