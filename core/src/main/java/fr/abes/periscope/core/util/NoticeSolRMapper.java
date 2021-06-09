package fr.abes.periscope.core.util;

import fr.abes.periscope.core.entity.*;
import fr.abes.periscope.core.entity.v1.NoticeV1;
import fr.abes.periscope.core.entity.v1.solr.NoticeV1Solr;
import fr.abes.periscope.core.entity.v2.NoticeV2;
import fr.abes.periscope.core.entity.v2.solr.ItemSolr;
import fr.abes.periscope.core.entity.v2.solr.NoticeV2Solr;
import fr.abes.periscope.core.entity.visualisation.Holding;
import fr.abes.periscope.core.entity.visualisation.NoticeVisu;
import fr.abes.periscope.core.entity.visualisation.SequenceContinue;
import fr.abes.periscope.core.entity.visualisation.SequenceLacune;
import fr.abes.periscope.core.entity.xml.DataField;
import fr.abes.periscope.core.entity.xml.NoticeXml;
import fr.abes.periscope.core.entity.xml.SubField;
import fr.abes.periscope.core.exception.IllegalHoldingException;
import fr.abes.periscope.core.exception.IllegalPublicationYearException;
import fr.abes.periscope.core.exception.MissingFieldException;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.EnumUtils;
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

    @Autowired
    private ModelMapper modelMapper;

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
                    target.setPcpList(source.getPcpList());
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
                        PublicationYear year = buildStartPublicationYear(source.getProcessingGlobalData());
                        target.setStartYear(year);
                    } catch (IllegalPublicationYearException e) {
                        log.debug("Unable to parse start publication year :" + e.getLocalizedMessage());
                        target.setStartYear(null);
                    }

                    // Extraction de la date de fin
                    try {
                        PublicationYear year = buildEndPublicationYear(source.getProcessingGlobalData());
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
        modelMapper.addConverter(myConverter);
    }

    /**
     * Convertisseur pour les notices SolR V2 vers les notices PERISCOPE
     */
    @Bean
    public void converterNoticeV2Solr() {

        Converter<NoticeV2Solr, Notice> myConverter = new Converter<NoticeV2Solr, Notice>() {

            public NoticeV2 convert(MappingContext<NoticeV2Solr, Notice> context) {
                NoticeV2Solr source = context.getSource();
                NoticeV2 target = new NoticeV2();

                try {

                    target.setPpn(source.getPpn());
                    target.setIssn((source.getIssn()));

                    Iterator<ItemSolr> itemIterator = source.getItems().iterator();
                    while(itemIterator.hasNext()) {
                        ItemSolr itemSolR = itemIterator.next();
                        Item item = new Item(itemSolR.getEpn());

                        item.setPpn(itemSolR.getPpn());
                        item.setRcr(itemSolR.getRcr());
                        item.setPcp(itemSolR.getPcp());

                        target.addItem(item);
                    }

                    target.setPublisher(source.getEditorForDisplay());
                    target.setKeyTitle(source.getKeyTitle());
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

                    return target;

                } catch (Exception ex) {
                    throw new MappingException(Arrays.asList(new ErrorMessage(ex.getMessage())));
                }
            }
        };
        modelMapper.addConverter(myConverter);
    }

    /**
     * Extrait l'année de début de publication
     * @param value zone
     * @return PublicationYear Année de début de publication
     * @throws IllegalPublicationYearException si l'année de publication ne peut pas être décodée
     */
    public PublicationYear buildStartPublicationYear(String value) throws IllegalPublicationYearException {
        String yearCode = value.substring(8, 9);
        String candidateYear;
        switch (yearCode) {
            case "b":
            case "a":
            case "c":
            case "d":
            case "e":
            case "g":
            case "h":
            case "i":
            case "j":
                candidateYear = value.substring(9, 13);
                return extractDate(candidateYear);
            case "f":
                String candidateOldestYear = value.substring(9, 13);
                String candidateNewestYear = value.substring(13, 17);
                return extractCaseF(candidateOldestYear, candidateNewestYear);
            default:
                throw new IllegalPublicationYearException("Unable to decode year code " + yearCode);
        }

    }

    /**
     * Extrait l'année de fin de publication
     * @param value zone
     * @return PublicationYear Année de fin de publication
     * @throws IllegalPublicationYearException si l'année de publication ne peut pas être décodée
     */
    public PublicationYear buildEndPublicationYear(String value) throws IllegalPublicationYearException {
        String yearCode = value.substring(8, 9);
        String candidateYear;

        switch (yearCode) {
            case "b":
                candidateYear = value.substring(13, 17);
                return extractDate(candidateYear);
            case "a":
                candidateYear = value.substring(13, 17);
                if (candidateYear.equals("9999")) {
                    return new PublicationYear(); // Année nulle par défaut
                } else
                    throw new IllegalPublicationYearException("Unable to decode end year code " + yearCode);
            case "c":
            case "d":
                candidateYear = value.substring(13, 17);
                if (candidateYear.equals("    ")) {
                    return new PublicationYear(); // Année nulle par défaut
                } else
                    throw new IllegalPublicationYearException("Unable to decode end year code " + yearCode);
            case "e":
            case "f":
            case "h":
            case "i":
            case "j":
                return new PublicationYear();
            case "g":
                candidateYear = value.substring(13, 17);
                if (candidateYear.equals("9999")) {
                    return new PublicationYear(); // Année nulle par défaut
                } else {
                    return extractDate(candidateYear);
                }
            default:
                throw new IllegalPublicationYearException("Unable to decode year code " + yearCode);
        }
    }

    /**
     * Extrait la date de publication
     * @param candidateYear
     * @return
     * @throws IllegalPublicationYearException
     */
    private PublicationYear extractDate(String candidateYear) throws IllegalPublicationYearException {
        PublicationYear year = new PublicationYear();
        if (candidateYear.equals("    ")) return year;
        if (candidateYear.charAt(2) == ' ' && candidateYear.charAt(3) == ' ') {
            year.setYear(candidateYear.substring(0, 2) + "XX");
            year.setConfidenceIndex(100);
        } else if (candidateYear.charAt(2) == ' ') {
            new IllegalPublicationYearException("Unable to decode year format like" + candidateYear);

        } else if (candidateYear.charAt(3) == ' ') {
            year.setYear(candidateYear.substring(0, 3) + "X");
            year.setConfidenceIndex(10);
        } else {
            year.setYear(candidateYear.substring(0, 4));
            year.setConfidenceIndex(0);
        }
        return year;
    }

    /**
     * Extrait le cas F
     * @param candidateOldestYear
     * @param candidateNewestYear
     * @return
     * @throws IllegalPublicationYearException
     */
    private PublicationYear extractCaseF(String candidateOldestYear, String candidateNewestYear) throws IllegalPublicationYearException {
        int cdtOldestYear = (candidateOldestYear.equals("    ")) ? 0 : Integer.parseInt(candidateOldestYear.trim());
        int cdtNewestYear = (candidateNewestYear.equals("    ")) ? 9999 : Integer.parseInt(candidateNewestYear.trim());
        PublicationYear year = new PublicationYear();
        if (cdtOldestYear > cdtNewestYear) {
            throw new IllegalPublicationYearException("Oldest Year can't be superior to newest Year");
        }
        if (cdtOldestYear == 0) {
            year.setYear(candidateNewestYear);
        } else {
            year.setYear(candidateOldestYear);
        }
        if (cdtNewestYear != 9999 && cdtOldestYear != 0)
            year.setConfidenceIndex(cdtNewestYear - cdtOldestYear);
        else
            year.setConfidenceIndex(0);
        return year;
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
