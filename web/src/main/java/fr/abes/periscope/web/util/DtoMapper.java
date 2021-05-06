package fr.abes.periscope.web.util;

import fr.abes.periscope.core.criterion.*;
import fr.abes.periscope.core.entity.v2.solr.NoticeV2SolrField;
import fr.abes.periscope.core.exception.CriterionOperatorMismatchException;
import fr.abes.periscope.core.exception.IllegalCriterionException;
import fr.abes.periscope.core.exception.IllegalOperatorException;
import fr.abes.periscope.core.entity.v1.solr.NoticeV1SolrField;
import fr.abes.periscope.core.exception.IllegalSortException;
import fr.abes.periscope.web.dto.criterion.*;
import org.modelmapper.Converter;
import org.modelmapper.ModelMapper;
import org.modelmapper.spi.MappingContext;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.stereotype.Service;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Utilitaire de mapping de objets Entité (core) et des objets DTO (API)
 */
@Service
public class DtoMapper {

    @Autowired
    private ModelMapper modelMapper;

    /**
     * Fonction de mapping générique pour un objet
     *
     * @param source      Objet source
     * @param targetClass Classe de l'objet cible
     * @return Objet cible
     */
    public <S, T> T map(S source, Class<T> targetClass) {
        return modelMapper.map(source, targetClass);
    }

    /**
     * Fonction de mapping générique pour des listes
     *
     * @param source      Liste source
     * @param targetClass Classe des objets cibles
     * @return Liste des objets cibles
     */
    public <S, T> List<T> mapList(List<S> source, Class<T> targetClass) {
        return source
                .stream()
                .map(element -> modelMapper.map(element, targetClass))
                .collect(Collectors.toList());
    }

    /** Convertisseur pour les critères de tri
     * On vérifie la definition des champs depuis la classe NoticeV1SolrField
     */
    @Bean
    public void converterSort() {
        Converter<CriterionSortWebDto, CriterionSort> myConverter = new Converter<CriterionSortWebDto, CriterionSort>() {
            @Override
            public CriterionSort convert(MappingContext<CriterionSortWebDto, CriterionSort> mappingContext) {
                CriterionSortWebDto s = mappingContext.getSource();
                final String[] field = {""};
                try {
                    Class c = NoticeV1SolrField.class;
                    if (s.getVersion().equalsIgnoreCase("v2")) {
                        c = NoticeV2SolrField.class;
                    }

                    Arrays.stream(c.getDeclaredFields()).forEach(n -> {
                        if (n.getName().equalsIgnoreCase(s.getSort())) {
                            try {
                                field[0] = String.valueOf(n.get(s.getSort()));
                            } catch (IllegalAccessException e) {
                                throw new IllegalSortException(s.getSort() + " : Critère de tri inconnu");
                            }
                        }
                    });
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
        modelMapper.addConverter(myConverter);
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
                        throw new IllegalArgumentException(CriterionTypeName.CRITERION_PCP + " - property '"+CriterionWebDto.OPERATOR_PROPERTY+"' is missing");
                    }
                    if (s.getPcp() == null) {
                        throw new IllegalArgumentException(CriterionTypeName.CRITERION_PCP + " - property '"+CriterionPcpWebDto.PCP_PROPERTY+"' is missing");
                    }
                    if (s.getPcpOperator() == null) {
                        throw new IllegalArgumentException(CriterionTypeName.CRITERION_PCP + " - property '"+CriterionPcpWebDto.PCP_OPERATOR_PROPERTY +"' is missing");
                    }

                    CriterionPcp d = new CriterionPcp(s.getBlocOperator(), s.getPcp(), s.getPcpOperator());
                    return d;

                } catch (IllegalOperatorException ex) {
                    throw new IllegalOperatorException(CriterionTypeName.CRITERION_PCP + " : " + ex.getLocalizedMessage());
                } catch (CriterionOperatorMismatchException ex) {
                    throw new CriterionOperatorMismatchException(CriterionTypeName.CRITERION_PCP + " - '"+CriterionPcpWebDto.PCP_PROPERTY+"' dismatch size of '"+CriterionPcpWebDto.PCP_OPERATOR_PROPERTY +"' : " + ex.getLocalizedMessage());
                } catch (IllegalCriterionException ex) {
                    throw new IllegalCriterionException(CriterionTypeName.CRITERION_PCP + " : " + ex.getLocalizedMessage());
                }
            }
        };
        modelMapper.addConverter(myConverter);
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
                        throw new IllegalArgumentException(CriterionTypeName.CRITERION_RCR + " - property '"+CriterionWebDto.OPERATOR_PROPERTY+"' is missing");
                    }
                    if (s.getRcr() == null) {
                        throw new IllegalArgumentException(CriterionTypeName.CRITERION_RCR + " - property '"+CriterionRcrWebDto.RCR_PROPERTY+"' is missing");
                    }
                    if (s.getRcrOperator() == null) {
                        throw new IllegalArgumentException(CriterionTypeName.CRITERION_RCR + " - property '"+CriterionRcrWebDto.RCR_OPERATOR_PROPERTY +"' is missing");
                    }

                    CriterionRcr d = new CriterionRcr(s.getBlocOperator(), s.getRcr(), s.getRcrOperator());
                    return d;
                } catch (IllegalOperatorException ex) {
                    throw new IllegalOperatorException(CriterionTypeName.CRITERION_RCR  + " : " + ex.getLocalizedMessage());
                } catch (CriterionOperatorMismatchException ex) {
                    throw new CriterionOperatorMismatchException(CriterionTypeName.CRITERION_RCR + " - '"+CriterionRcrWebDto.RCR_PROPERTY+"' dismatch size of '"+CriterionRcrWebDto.RCR_OPERATOR_PROPERTY +"' : " + ex.getLocalizedMessage());
                } catch (IllegalCriterionException ex) {
                    throw new IllegalCriterionException(CriterionTypeName.CRITERION_RCR + " : " + ex.getLocalizedMessage());
                }
            }
        };
        modelMapper.addConverter(myConverter);
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
                        throw new IllegalArgumentException(CriterionTypeName.CRITERION_PPN + " - property '"+CriterionWebDto.OPERATOR_PROPERTY+"' is missing");
                    }
                    if (s.getPpn() == null) {
                        throw new IllegalArgumentException(CriterionTypeName.CRITERION_PPN + " - property '"+CriterionPpnWebDto.PPN_PROPERTY+"' is missing");
                    }

                    CriterionPpn d = new CriterionPpn(s.getBlocOperator(), s.getPpn());
                    return d;
                } catch (IllegalOperatorException ex) {
                    throw new IllegalOperatorException(CriterionTypeName.CRITERION_PPN  + " : " + ex.getLocalizedMessage());
                } catch (CriterionOperatorMismatchException ex) {
                    throw new CriterionOperatorMismatchException(CriterionTypeName.CRITERION_PPN + " - '"+CriterionPpnWebDto.PPN_PROPERTY+"' dismatch size of '"+CriterionPpnWebDto.PPN_OPERATOR_PROPERTY +"' : " + ex.getLocalizedMessage());
                } catch (IllegalCriterionException ex) {
                    throw new IllegalCriterionException(CriterionTypeName.CRITERION_PPN + " : " + ex.getLocalizedMessage());
                }
            }
        };
        modelMapper.addConverter(myConverter);
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
                        throw new IllegalArgumentException(CriterionTypeName.CRITERION_TITLE_WORDS + " - property '"+CriterionWebDto.OPERATOR_PROPERTY+"' is missing");
                    }
                    if (s.getTitleWords() == null) {
                        throw new IllegalArgumentException(CriterionTypeName.CRITERION_TITLE_WORDS + " - property '"+CriterionTitleWordsWebDto.TITLE_WORDS_PROPERTY+"' is missing");
                    }
                    if (s.getTitleWordsOperator() == null) {
                        throw new IllegalArgumentException(CriterionTypeName.CRITERION_TITLE_WORDS + " - property '"+CriterionTitleWordsWebDto.TITLE_WORDS_OPERATOR_PROPERTY +"' is missing");
                    }

                    CriterionTitleWords d = new CriterionTitleWords(s.getBlocOperator(), s.getTitleWords(), s.getTitleWordsOperator());
                    return d;
                } catch (IllegalOperatorException ex) {
                    throw new IllegalOperatorException(CriterionTypeName.CRITERION_TITLE_WORDS  + " : " + ex.getLocalizedMessage());
                } catch (CriterionOperatorMismatchException ex) {
                    throw new CriterionOperatorMismatchException(CriterionTypeName.CRITERION_TITLE_WORDS + " - '"+CriterionTitleWordsWebDto.TITLE_WORDS_PROPERTY+"' dismatch size of '"+CriterionTitleWordsWebDto.TITLE_WORDS_OPERATOR_PROPERTY +"' : " + ex.getLocalizedMessage());
                } catch (IllegalCriterionException ex) {
                    throw new IllegalCriterionException(CriterionTypeName.CRITERION_TITLE_WORDS + " : " + ex.getLocalizedMessage());
                }
            }
        };
        modelMapper.addConverter(myConverter);
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
                        throw new IllegalArgumentException(CriterionTypeName.CRITERION_ISSN + " - property '"+CriterionWebDto.OPERATOR_PROPERTY+"' is missing");
                    }
                    if (s.getIssn() == null) {
                        throw new IllegalArgumentException(CriterionTypeName.CRITERION_ISSN + " - property '"+CriterionIssnWebDto.ISSN_PROPERTY+"' is missing");
                    }

                    CriterionIssn d = new CriterionIssn(s.getBlocOperator(), s.getIssn());
                    return d;
                } catch (IllegalOperatorException ex) {
                    throw new IllegalOperatorException(CriterionTypeName.CRITERION_ISSN  + " : " + ex.getLocalizedMessage());
                } catch (CriterionOperatorMismatchException ex) {
                    throw new CriterionOperatorMismatchException(CriterionTypeName.CRITERION_ISSN + " - '"+CriterionIssnWebDto.ISSN_PROPERTY+"' dismatch size of '"+CriterionIssnWebDto.ISSN_OPERATOR_PROPERTY +"' : " + ex.getLocalizedMessage());
                } catch (IllegalCriterionException ex) {
                    throw new IllegalCriterionException(CriterionTypeName.CRITERION_ISSN + " : " + ex.getLocalizedMessage());
                }
            }
        };
        modelMapper.addConverter(myConverter);
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
                        throw new IllegalArgumentException(CriterionTypeName.CRITERION_EDITOR + " - property '"+CriterionWebDto.OPERATOR_PROPERTY+"' is missing");
                    }
                    if (s.getEditors() == null) {
                        throw new IllegalArgumentException(CriterionTypeName.CRITERION_EDITOR + " - property '"+CriterionEditorWebDto.EDITORS_PROPERTY+"' is missing");
                    }
                    if (s.getEditorsOperator() == null) {
                        throw new IllegalArgumentException(CriterionTypeName.CRITERION_EDITOR + " - property '"+CriterionEditorWebDto.EDITORS_OPERATOR_PROPERTY+"' is missing");
                    }

                    CriterionEditor d = new CriterionEditor(s.getBlocOperator(), s.getEditors(), s.getEditorsOperator());
                    return d;
                } catch (IllegalOperatorException ex) {
                    throw new IllegalOperatorException(CriterionTypeName.CRITERION_EDITOR  + " : " + ex.getLocalizedMessage());
                } catch (CriterionOperatorMismatchException ex) {
                    throw new CriterionOperatorMismatchException(CriterionTypeName.CRITERION_EDITOR + " - '"+CriterionEditorWebDto.EDITORS_PROPERTY+"' dismatch size of '"+CriterionEditorWebDto.EDITORS_OPERATOR_PROPERTY+"' : " + ex.getLocalizedMessage());
                } catch (IllegalCriterionException ex) {
                    throw new IllegalCriterionException(CriterionTypeName.CRITERION_EDITOR + " : " + ex.getLocalizedMessage());
                }
            }
        };
        modelMapper.addConverter(myConverter);
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
                        throw new IllegalArgumentException(CriterionTypeName.CRITERION_COUNTRIES + " - property '"+CriterionWebDto.OPERATOR_PROPERTY+"' is missing");
                    }
                    if (s.getCountries() == null) {
                        throw new IllegalArgumentException(CriterionTypeName.CRITERION_COUNTRIES + " - property '"+CriterionCountryWebDto.COUNTRIES_PROPERTY+"' is missing");
                    }
                    if (s.getCountriesOperator() == null) {
                        throw new IllegalArgumentException(CriterionTypeName.CRITERION_COUNTRIES + " - property '"+CriterionCountryWebDto.COUNTRIES_OPERATOR_PROPERTY+"' is missing");
                    }

                    CriterionCountry d = new CriterionCountry(s.getBlocOperator(), s.getCountries(), s.getCountriesOperator());
                    return d;
                } catch (IllegalOperatorException ex) {
                    throw new IllegalOperatorException(CriterionTypeName.CRITERION_COUNTRIES  + " : " + ex.getLocalizedMessage());
                } catch (CriterionOperatorMismatchException ex) {
                    throw new CriterionOperatorMismatchException(CriterionTypeName.CRITERION_COUNTRIES + " - '"+CriterionCountryWebDto.COUNTRIES_PROPERTY+"' dismatch size of '"+CriterionCountryWebDto.COUNTRIES_OPERATOR_PROPERTY+"' : " + ex.getLocalizedMessage());
                } catch (IllegalCriterionException ex) {
                    throw new IllegalCriterionException(CriterionTypeName.CRITERION_COUNTRIES + " : " + ex.getLocalizedMessage());
                }
            }
        };
        modelMapper.addConverter(myConverter);
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
                        throw new IllegalArgumentException(CriterionTypeName.CRITERION_LANGUAGE + " - property '"+CriterionWebDto.OPERATOR_PROPERTY+"' is missing");
                    }
                    if (s.getLanguage() == null) {
                        throw new IllegalArgumentException(CriterionTypeName.CRITERION_LANGUAGE + " - property '"+CriterionLanguageWebDto.LANGUAGE_PROPERTY+"' is missing");
                    }
                    if (s.getLanguageOperators() == null) {
                        throw new IllegalArgumentException(CriterionTypeName.CRITERION_LANGUAGE + " - property '"+CriterionLanguageWebDto.LANGUAGE_OPERATOR_PROPERTY+"' is missing");
                    }

                    CriterionLanguage d = new CriterionLanguage(s.getBlocOperator(), s.getLanguage(), s.getLanguageOperators());
                    return d;
                } catch (IllegalOperatorException ex) {
                    throw new IllegalOperatorException(CriterionTypeName.CRITERION_LANGUAGE  + " : " + ex.getLocalizedMessage());
                } catch (CriterionOperatorMismatchException ex) {
                    throw new CriterionOperatorMismatchException(CriterionTypeName.CRITERION_LANGUAGE + " - '"+CriterionLanguageWebDto.LANGUAGE_PROPERTY+"' dismatch size of '"+CriterionLanguageWebDto.LANGUAGE_OPERATOR_PROPERTY+"' : " + ex.getLocalizedMessage());
                } catch (IllegalCriterionException ex) {
                    throw new IllegalCriterionException(CriterionTypeName.CRITERION_LANGUAGE + " : " + ex.getLocalizedMessage());
                }
            }
        };
        modelMapper.addConverter(myConverter);
    }
}
