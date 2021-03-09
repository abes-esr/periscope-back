package fr.abes.periscope.core.util;

import fr.abes.periscope.core.entity.Notice;
import fr.abes.periscope.core.entity.NoticeSolr;
import fr.abes.periscope.core.entity.OnGoingResourceType;
import fr.abes.periscope.core.entity.PublicationYear;
import fr.abes.periscope.core.exception.IllegalPublicationYearException;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.stream.Collectors;

@Component
@Slf4j
public class NoticeMapper {

    @Autowired
    private ModelMapper modelMapper;

    public PublicationYear buildStartPublicationYear(String value) throws IllegalPublicationYearException {
        //log.debug("SolR startdate : "+value.substring(9,13));
        String yearCode = value.substring(8, 9);
        String candidateYear;
        PublicationYear year = new PublicationYear();
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
            new IllegalPublicationYearException("Unable to decode year format like" + candidateYear);

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

    public List<Notice> mapList(List<NoticeSolr> source) {
        return source
                .stream()
                .map(element -> map(element))
                .collect(Collectors.toList());
    }

    public Notice map(NoticeSolr source) {
        Notice notice = modelMapper.map(source, Notice.class);

        // Extraction de la date de début
        try {
            PublicationYear year = buildStartPublicationYear(source.getProcessingGlobalData());
            notice.setStartYear(year);
        } catch (IllegalPublicationYearException e) {
            log.debug("Unable to parse start publication year :" + e.getLocalizedMessage());
            notice.setStartYear(null);
        }

        // Extraction de la date de fin
        try {
            PublicationYear year = buildEndPublicationYear(source.getProcessingGlobalData());
            notice.setEndYear(year);
        } catch (IllegalPublicationYearException e) {
            log.debug("Unable to parse end publication year :" + e.getLocalizedMessage());
            notice.setEndYear(null);
        }

        //Extraction du type de ressource continue
        notice.setContiniousType(extractOnGoingResourceType(source.getContiniousType()));

        //Extraction du lien exterieur de Mirabel
        notice.setMirabelURL(extractMirabelURL(source.getExternalURLs()));

        return notice;
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
