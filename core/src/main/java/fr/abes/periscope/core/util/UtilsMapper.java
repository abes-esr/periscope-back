package fr.abes.periscope.core.util;

import fr.abes.periscope.core.entity.solr.PublicationYear;
import fr.abes.periscope.core.entity.solr.SupportType;
import fr.abes.periscope.core.exception.IllegalPublicationYearException;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Convertisseurs entre les NoticeSolR et les Notices pour PERISCOPE
 */
@Component
@Slf4j
public class UtilsMapper extends ModelMapper {

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
                .map(element -> this.map(element, targetClass))
                .collect(Collectors.toList());
    }

    /**
     * Fonction de mapping générique pour des sets
     *
     * @param source      Liste source
     * @param targetClass Classe des objets cibles
     * @return Liste des objets cibles
     */
    public <S, T> Set<T> mapSet(Set<S> source, Class<T> targetClass) {
        return source
                .stream()
                .map(element -> this.map(element, targetClass))
                .collect(Collectors.toSet());
    }

    /**
     * Extrait la date de publication
     * @param candidateYear
     * @return
     * @throws IllegalPublicationYearException
     */
    public PublicationYear extractDate(String candidateYear) throws IllegalPublicationYearException {
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
     * Extrait le cas F
     * @param candidateOldestYear
     * @param candidateNewestYear
     * @return
     * @throws IllegalPublicationYearException
     */
    protected PublicationYear extractCaseF(String candidateOldestYear, String candidateNewestYear) throws IllegalPublicationYearException {
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


    public String getTitre(String titreCle, String titreCleQualifie, String titreCleCourt, String titrePropre, String titreAuteurDifferent, String titreParallele, String titreComplement) {
        String titre = titreCle;
        if (titre != null && !titre.isEmpty() && titreCleQualifie != null) {
            titre += " " + titreCleQualifie;
            return titre;
        }
        if (titre == null || titre.isEmpty()) {
            if (titreCleCourt != null && !titreCleCourt.isEmpty()) {
                return titreCleCourt;
            }

            if (titrePropre != null && !titrePropre.isEmpty()) {
                return titrePropre;
            }

            if (titreAuteurDifferent != null && !titreAuteurDifferent.isEmpty()) {
                return titreAuteurDifferent;
            }

            if (titreParallele != null && !titreParallele.isEmpty()) {
                return titreParallele;
            }

            if (titreComplement != null && !titreComplement.isEmpty()) {
                return titreComplement;
            }
        }
        if (titre != null) {
            //suppression des caractères entourant les mots vides pour l'affichage
            return titre.replace("\u0098", "").replace("\u009c", "");
        }
        return null;
    }

    public String extractSupportType(String typeSupport) {
        if (typeSupport == null) {
            return SupportType.X;
        }
        switch (typeSupport) {
            case "a":
                return SupportType.A;
            case "b":
                return SupportType.B;
            case "c":
                return SupportType.C;
            case "d":
                return SupportType.D;
            case "e":
                return SupportType.E;
            case "f":
                return SupportType.F;
            case "g":
                return SupportType.G;
            case "i":
                return SupportType.I;
            case "j":
                return SupportType.J;
            case "l":
                return SupportType.L;
            case "m":
                return SupportType.M;
            case "r":
                return SupportType.R;
            default:
                return SupportType.X;
        }
    }
}
