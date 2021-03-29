package fr.abes.periscope.core.util;

import fr.abes.periscope.core.entity.Notice;
import fr.abes.periscope.core.entity.OnGoingResourceType;
import fr.abes.periscope.core.entity.PublicationYear;
import fr.abes.periscope.core.entity.v1.NoticeV1;
import fr.abes.periscope.core.entity.v1.solr.NoticeV1Solr;
import fr.abes.periscope.core.entity.v2.NoticeV2;
import fr.abes.periscope.core.entity.v2.solr.ItemSolr;
import fr.abes.periscope.core.entity.v2.solr.NoticeV2Solr;
import fr.abes.periscope.core.exception.IllegalPublicationYearException;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.Converter;
import org.modelmapper.MappingException;
import org.modelmapper.ModelMapper;
import org.modelmapper.spi.ErrorMessage;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.stereotype.Component;

import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;

@Component
@Slf4j
public class NoticeMapper {

    @Bean
    public ModelMapper modelMapper() {
        return new ModelMapper();
    }

    @Autowired
    private ModelMapper modelMapper;

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
     * Convertisseur pour les notices SolR V1 vers les notices PERISCOPE
     */
    @Bean
    public void converterNoticeV1Solr() {

        Converter<NoticeV1Solr, Notice> myConverter = context -> {
            NoticeV1Solr source = context.getSource();
            Notice target = new NoticeV1();

            try {

                target.setPpn(source.getPpn());
                target.setIssn((source.getIssn()));
                target.setPcpList(source.getPcpList());
                target.setRcrList(source.getRcrList());
                extractCommonFields(target, source.getEditor(), source.getKeyTitle(), source.getKeyShortedTitle(), source.getProperTitle(), source.getTitleFromDifferentAuthor(), source.getParallelTitle(), source.getTitleComplement(), source.getSectionTitle());

                // Extraction de la date de début
                extractStartYear(source, target);

                // Extraction de la date de fin
                extractEndYear(source, target);

                //Extraction du type de ressource continue
                target.setContiniousType(extractOnGoingResourceType(source.getContiniousType()));

                //Extraction du lien exterieur de Mirabel
                target.setMirabelURL(extractMirabelURL(source.getExternalURLs()));

                target.setNbLocation(source.getNbLocation());

                return target;

            } catch (Exception ex) {
                throw new MappingException(Arrays.asList(new ErrorMessage(ex.getMessage())));
            }
        };
        modelMapper.addConverter(myConverter);
    }

    private void extractCommonFields(Notice target, String editor, String keyTitle, String keyShortedTitle, String properTitle, String titleFromDifferentAuthor, String parallelTitle, String titleComplement, String sectionTitle) {
        target.setEditor(editor);
        target.setKeyTitle(keyTitle);
        target.setKeyShortedTitle(keyShortedTitle);
        target.setProperTitle(properTitle);
        target.setTitleFromDifferentAuthor(titleFromDifferentAuthor);
        target.setParallelTitle(parallelTitle);
        target.setTitleComplement(titleComplement);
        target.setSectionTitle(sectionTitle);
    }

    private void extractStartYear(NoticeV1Solr source, Notice target) {
        try {
            PublicationYear year = buildStartPublicationYear(source.getProcessingGlobalData());
            target.setStartYear(year);
        } catch (IllegalPublicationYearException e) {
            log.debug("Unable to parse start publication year :" + e.getLocalizedMessage());
            target.setStartYear(null);
        }
    }

    private void extractEndYear(NoticeV1Solr source, Notice target) {
        try {
            PublicationYear year = buildEndPublicationYear(source.getProcessingGlobalData());
            target.setEndYear(year);
        } catch (IllegalPublicationYearException e) {
            log.debug("Unable to parse end publication year :" + e.getLocalizedMessage());
            target.setEndYear(null);
        }
    }

    /**
     * Convertisseur pour les notices SolR V1 vers les notices PERISCOPE
     */
    @Bean
    public void converterNoticeV2Solr() {

        Converter<NoticeV2Solr, Notice> myConverter = context -> {
            NoticeV2Solr source = context.getSource();
            Notice target = new NoticeV2();

            try {

                target.setPpn(source.getPpn());
                target.setIssn((source.getIssn()));

                Iterator<ItemSolr> itemIterator = source.getItems().iterator();
                while(itemIterator.hasNext()) {
                    ItemSolr item = itemIterator.next();

                    target.getPcpList().add(item.getEpn());
                    target.getRcrList().add(item.getRcr());
                }

                extractCommonFields(target, source.getEditor(), source.getKeyTitle(), source.getKeyShortedTitle(), source.getProperTitle(), source.getTitleFromDifferentAuthor(), source.getParallelTitle(), source.getTitleComplement(), source.getSectionTitle());

                //Extraction du lien exterieur de Mirabel
                target.setMirabelURL(extractMirabelURL(source.getExternalURLs()));

                target.setNbLocation(source.getNbLocation());

                return target;

            } catch (Exception ex) {
                throw new MappingException(Arrays.asList(new ErrorMessage(ex.getMessage())));
            }
        };
        modelMapper.addConverter(myConverter);
    }

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

    private PublicationYear extractDate(String candidateYear) {
        PublicationYear year = new PublicationYear();
        if (candidateYear.charAt(2) == ' ' && candidateYear.charAt(3) == ' ') {
            year.setYear(Integer.valueOf(candidateYear.substring(0, 2)));
            year.setConfidenceIndex(100);
        } else if (candidateYear.charAt(2) == ' ') {
            throw new IllegalPublicationYearException("Unable to decode year format like" + candidateYear);

        } else if (candidateYear.charAt(3) == ' ') {
            year.setYear(Integer.valueOf(candidateYear.substring(0, 3)));
            year.setConfidenceIndex(10);
        } else {
            year.setYear(Integer.valueOf(candidateYear.substring(0, 4)));
            year.setConfidenceIndex(0);
        }
        return year;
    }

    private PublicationYear extractCaseF(String candidateOldestYear, String candidateNewestYear) {
        int cdtOldestYear = Integer.parseInt(candidateOldestYear.trim());
        int cdtNewestYear = (candidateNewestYear.equals("    ")) ? 9999 : Integer.parseInt(candidateNewestYear.trim());
        PublicationYear year = new PublicationYear();
        if (cdtOldestYear > cdtNewestYear) {
            throw new IllegalPublicationYearException("Oldest Year can't be superior to newest Year");
        }
        year.setYear(cdtOldestYear);
        if (cdtNewestYear != 9999)
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
