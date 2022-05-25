package fr.abes.periscope.core.util;

import fr.abes.periscope.core.entity.solr.Item;
import fr.abes.periscope.core.entity.solr.Notice;
import fr.abes.periscope.core.entity.solr.OnGoingResourceType;
import fr.abes.periscope.core.entity.solr.PublicationYear;
import fr.abes.periscope.core.entity.solr.v1.NoticeV1;
import fr.abes.periscope.core.entity.solr.v1.NoticeV1Solr;
import fr.abes.periscope.core.entity.solr.v2.NoticeV2;
import fr.abes.periscope.core.entity.solr.v2.NoticeV2Solr;
import fr.abes.periscope.core.exception.IllegalPublicationYearException;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.Converter;
import org.modelmapper.MappingException;
import org.modelmapper.ModelMapper;
import org.modelmapper.spi.ErrorMessage;
import org.modelmapper.spi.MappingContext;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.stereotype.Component;

import java.util.*;

/**
 * Convertisseurs entre les notices issues de la base XML et les notices pour PERISCOPE
 */
@Component
@Slf4j
public class NoticeSolRMapper {
    private final UtilsMapper utilsMapper;

    @Autowired
    public NoticeSolRMapper(UtilsMapper utilsMapper) { this.utilsMapper = utilsMapper; }

    /**
     * Convertisseur pour les notices SolR V1 vers les notices PERISCOPE
     */
    @Bean
    public void converterNoticeV1Solr() {

        Converter<NoticeV1Solr, Notice> myConverter = new Converter<NoticeV1Solr, Notice>() {

            public NoticeV1 convert(MappingContext<NoticeV1Solr, Notice> context) {
                NoticeV1Solr source = context.getSource();
                NoticeV1 target = new NoticeV1();

                try {

                    target.setPpn(source.getPpn());
                    target.setIssn((source.getIssn()));
                    target.setRcrList(source.getRcrList());
                    target.setPublisher(source.getEditor());
                    target.setKeyTitle(source.getKeyTitle());
                    target.setKeyShortedTitle(source.getKeyShortedTitle());
                    target.setProperTitle(source.getProperTitle());
                    target.setTitleFromDifferentAuthor(source.getTitleFromDifferentAuthor());
                    target.setParallelTitle(source.getParallelTitle());
                    target.setTitleComplement(source.getTitleComplement());
                    target.setSectionTitle(source.getSectionTitle());

                    // Extraction de la date de début
                    try {
                        PublicationYear year = utilsMapper.buildStartPublicationYear(source.getProcessingGlobalData());
                        target.setStartYear(year);
                    } catch (IllegalPublicationYearException e) {
                        log.debug("Unable to parse start publication year :" + e.getLocalizedMessage());
                        target.setStartYear(null);
                    }

                    // Extraction de la date de fin
                    try {
                        PublicationYear year = utilsMapper.buildEndPublicationYear(source.getProcessingGlobalData());
                        target.setEndYear(year);
                    } catch (IllegalPublicationYearException e) {
                        log.debug("Unable to parse end publication year :" + e.getLocalizedMessage());
                        target.setEndYear(null);
                    }

                    //Extraction du type de ressource continue
                    target.setContinuousType(extractOnGoingResourceType(source.getContiniousType()));

                    //Extraction du lien exterieur de Mirabel
                    target.setMirabelURL(extractMirabelURL(source.getExternalURLs()));

                    target.setNbLocation(source.getNbLocation());

                    return target;

                } catch (Exception ex) {
                    throw new MappingException(Arrays.asList(new ErrorMessage(ex.getMessage())));
                }
            }
        };
        utilsMapper.addConverter(myConverter);
    }

    /**
     * Convertisseur pour les notices SolR V2 vers les notices PERISCOPE
     */
    @Bean
    public void converterNoticeV2Solr() {

        Converter<NoticeV2Solr, Notice> myConverter = new Converter<NoticeV2Solr, Notice>() {
            @Override
            public Notice convert(MappingContext<NoticeV2Solr, Notice> context) {
                NoticeV2Solr source = context.getSource();
                NoticeV2 target = new NoticeV2();

                try {

                    target.setPpn(source.getPpn());
                    target.setIssn((source.getIssn()));
                    target.setPublisher(source.getEditorForDisplay());
                    target.setKeyTitle(source.getKeyTitle());
                    target.setKeyTitleQualifer(source.getKeyTitleQualifer());
                    target.setKeyShortedTitle(source.getKeyShortedTitleForDisplay());
                    target.setProperTitle(source.getProperTitleForDisplay());
                    target.setTitleFromDifferentAuthor(source.getTitleFromDifferentAuthorForDisplay());
                    target.setParallelTitle(source.getParallelTitleForDisplay());
                    target.setTitleComplement(source.getTitleComplementForDisplay());
                    target.setSectionTitle(source.getSectionTitleForDisplay());
                    target.setContinuousType(source.getContinuousType());
                    target.setSupportType(source.getSupportType());
                    target.setLanguage(source.getLanguage());
                    target.setCountry(source.getCountry());

                    if(source.getStartYear() != null ) {
                        target.setStartYear(new PublicationYear(source.getStartYear(), source.getStartYearConfidenceIndex()));
                    }
                    if(source.getEndYear() != null ) {
                        target.setEndYear(new PublicationYear(source.getEndYear(), source.getEndYearConfidenceIndex()));
                    }

                    //Extraction du lien exterieur de Mirabel
                    target.setMirabelURL(extractMirabelURL(source.getExternalURLs()));

                    target.setNbLocation(source.getNbLocation());
                    target.setNbPcp(source.getNbPcp());
                    target.setPcpList(source.getPcpList());

                    source.getItems().forEach(itemSolr -> {
                        //NB : la 930 $b est obligatoire, mais certains cas ont remonté une absence de rcr
                        if (itemSolr.getRcr() != null) {
                            Item item = new Item(itemSolr.getEpn());
                            item.setPpn(itemSolr.getPpn());
                            item.setRcr(itemSolr.getRcr());
                            item.setPcp(itemSolr.getPcp());

                            target.addItem(item);
                        }
                    });

                    return target;

                } catch (Exception ex) {
                    throw new MappingException(Arrays.asList(new ErrorMessage(ex.getMessage())));
                }
            }
        };
        utilsMapper.addConverter(myConverter);
    }




    /**
     * Extrait l'URL vers MIRABEL
     * @param externalURLs Liste des URL externes
     * @return String URL MIRABEL ou null
     */
    public String extractMirabelURL(List<String> externalURLs) {
        if (externalURLs == null) {
            return null;
        }
        return externalURLs.stream()
                .filter(link -> link.contains("mirabel"))
                .findFirst()
                .orElse(null);
    }

    /**
     * Extrait le type de ressource continue
     * @param continiousType
     * @return String Type de ressource continue
     */
    public String extractOnGoingResourceType(String continiousType) {

        if (continiousType == null) {
            return OnGoingResourceType.X;
        }

        switch (continiousType.substring(0,1)) {
            case "a":
                return OnGoingResourceType.A;
            case "b":
                return OnGoingResourceType.B;
            case "c":
                return OnGoingResourceType.C;
            case "e":
                return OnGoingResourceType.E;
            case "f":
                return OnGoingResourceType.F;
            case "g":
                return OnGoingResourceType.G;
            case "z":
                return OnGoingResourceType.Z;
            default:
                return OnGoingResourceType.X;
        }
    }
}
