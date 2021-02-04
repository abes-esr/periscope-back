package fr.abes.periscope.web.util;

import fr.abes.periscope.core.criterion.*;
import fr.abes.periscope.core.exception.CriterionOperatorMismatchException;
import fr.abes.periscope.core.exception.IllegalCriterionException;
import fr.abes.periscope.core.exception.IllegalOperatorException;
import fr.abes.periscope.core.util.TrackExecutionTime;
import fr.abes.periscope.web.dto.*;
import org.modelmapper.Converter;
import org.modelmapper.ModelMapper;
import org.modelmapper.spi.MappingContext;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.stream.Collectors;

@Service
public class DtoMapper {

    @Autowired
    private ModelMapper modelMapper;

    @Bean
    public ModelMapper modelMapper() {
        return new ModelMapper();
    }

    /**
     * Fonction de mapping générique pour des listes
     *
     * @param source      Liste source
     * @param targetClass Classe des objets cibles
     * @return Liste des objets cibles
     */
    @TrackExecutionTime
    public <S, T> List<T> mapList(List<S> source, Class<T> targetClass) {
        return source
                .stream()
                .map(element -> modelMapper.map(element, targetClass))
                .collect(Collectors.toList());
    }

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
     * Convertisseur pour les critères PCP (DTO vers objet métier)
     */
    @Bean
    public void converterPcp() {

        Converter<CriterionPcpWebDto, CriterionPcp> myConverter = new Converter<CriterionPcpWebDto, CriterionPcp>() {

            public CriterionPcp convert(MappingContext<CriterionPcpWebDto, CriterionPcp> context) {
                CriterionPcpWebDto s = context.getSource();
                try {
                    CriterionPcp d = new CriterionPcp(s.getBlocOperator(), s.getPcp(), s.getPcpOperator());
                    return d;
                } catch (IllegalOperatorException ex) {
                    throw new IllegalOperatorException(CriterionTypeName.CRITERION_PCP + ": " + ex.getLocalizedMessage());
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
                    CriterionRcr d = new CriterionRcr(s.getBlocOperator(), s.getRcr(), s.getRcrOperator());
                    return d;
                } catch (IllegalOperatorException ex) {
                    throw new IllegalOperatorException(CriterionTypeName.CRITERION_RCR + ": " + ex.getLocalizedMessage());
                } catch (CriterionOperatorMismatchException ex) {
                    throw new CriterionOperatorMismatchException(CriterionTypeName.CRITERION_RCR + ": " + ex.getLocalizedMessage());
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
                    CriterionPpn d = new CriterionPpn(s.getBlocOperator(), s.getPpn());
                    return d;
                } catch (IllegalOperatorException ex) {
                    throw new IllegalOperatorException(CriterionTypeName.CRITERION_PPN + ": " + ex.getLocalizedMessage());
                } catch (CriterionOperatorMismatchException ex) {
                    throw new CriterionOperatorMismatchException(CriterionTypeName.CRITERION_PPN + ": " + ex.getLocalizedMessage());
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
                    CriterionTitleWords d = new CriterionTitleWords(s.getBlocOperator(), s.getTitleWords(), s.getTitleWordsOperator());
                    return d;
                } catch (IllegalOperatorException ex) {
                    throw new IllegalOperatorException(CriterionTypeName.CRITERION_TITLE_WORDS + ": " + ex.getLocalizedMessage());
                } catch (CriterionOperatorMismatchException ex) {
                    throw new CriterionOperatorMismatchException(CriterionTypeName.CRITERION_TITLE_WORDS + ": " + ex.getLocalizedMessage());
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
                    CriterionIssn d = new CriterionIssn(s.getBlocOperator(), s.getIssn());
                    return d;
                } catch (IllegalOperatorException ex) {
                    throw new IllegalOperatorException(CriterionTypeName.CRITERION_ISSN + ": " + ex.getLocalizedMessage());
                } catch (CriterionOperatorMismatchException ex) {
                    throw new CriterionOperatorMismatchException(CriterionTypeName.CRITERION_ISSN + ": " + ex.getLocalizedMessage());
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
                    CriterionEditor d = new CriterionEditor(s.getBlocOperator(), s.getEditors(), s.getEditorsOperator());
                    return d;
                } catch (IllegalOperatorException ex) {
                    throw new IllegalOperatorException(CriterionTypeName.CRITERION_EDITOR+": "+ex.getLocalizedMessage());
                } catch (CriterionOperatorMismatchException ex) {
                    throw new CriterionOperatorMismatchException(CriterionTypeName.CRITERION_EDITOR+": "+ex.getLocalizedMessage());
                } catch (IllegalCriterionException ex) {
                    throw new IllegalCriterionException(CriterionTypeName.CRITERION_EDITOR+": "+ex.getLocalizedMessage());
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
        Converter<CriterionLangueWebDto, CriterionLangue> myConverter = new Converter<CriterionLangueWebDto, CriterionLangue>() {
            public CriterionLangue convert(MappingContext<CriterionLangueWebDto, CriterionLangue> context) {
                CriterionLangueWebDto s = context.getSource();
                try {
                    CriterionLangue d = new CriterionLangue(s.getBlocOperator(), s.getLangue(), s.getLangueOperator());
                    return d;
                } catch (IllegalOperatorException ex) {
                    throw new IllegalOperatorException(CriterionTypeName.CRITERION_LANGUE+": "+ex.getLocalizedMessage());
                } catch (CriterionOperatorMismatchException ex) {
                    throw new CriterionOperatorMismatchException(CriterionTypeName.CRITERION_LANGUE+": "+ex.getLocalizedMessage());
                }
            }
        };
        modelMapper.addConverter(myConverter);
    }
}
