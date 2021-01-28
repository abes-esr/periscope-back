package fr.abes.periscope.web.util;

import fr.abes.periscope.core.criterion.CriterionPcp;
import fr.abes.periscope.core.criterion.CriterionRcr;
import fr.abes.periscope.core.exception.CriterionOperatorMismatchException;
import fr.abes.periscope.core.exception.IllegalOperatorException;
import fr.abes.periscope.core.util.TrackExecutionTime;
import fr.abes.periscope.web.dto.CriterionPcpWebDto;
import fr.abes.periscope.web.dto.CriterionRcrWebDto;
import fr.abes.periscope.web.dto.CriterionTypeName;
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
     * @param source Liste source
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
     * @param source Objet source
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
                    CriterionPcp d = new CriterionPcp(s.getBlocOperator(), s.getPcp());
                    return d;
                } catch (IllegalOperatorException ex) {
                    throw new IllegalOperatorException(CriterionTypeName.CRITERION_PCP+": "+ex.getLocalizedMessage());
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
                    throw new IllegalOperatorException(CriterionTypeName.CRITERION_RCR+": "+ex.getLocalizedMessage());
                } catch (CriterionOperatorMismatchException ex) {
                    throw new CriterionOperatorMismatchException(CriterionTypeName.CRITERION_RCR+": "+ex.getLocalizedMessage());
                }
            }
        };
        modelMapper.addConverter(myConverter);
    }

}