package fr.abes.periscope.core.util;

import fr.abes.periscope.core.entity.solr.OnGoingResourceType;
import fr.abes.periscope.core.entity.solr.PublicationYear;
import fr.abes.periscope.core.entity.solr.SupportType;
import fr.abes.periscope.core.entity.visualisation.*;
import fr.abes.periscope.core.entity.xml.DataField;
import fr.abes.periscope.core.entity.xml.NoticeXml;
import fr.abes.periscope.core.entity.xml.SubField;
import fr.abes.periscope.core.exception.IllegalDateException;
import fr.abes.periscope.core.exception.IllegalHoldingException;
import fr.abes.periscope.core.exception.IllegalPublicationYearException;
import fr.abes.periscope.core.exception.MissingFieldException;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.Converter;
import org.modelmapper.MappingException;
import org.modelmapper.ModelMapper;
import org.modelmapper.spi.ErrorMessage;
import org.modelmapper.spi.MappingContext;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.stereotype.Component;

import java.time.Period;
import java.util.*;

/**
 * Convertisseurs entre les notices issues de la base XML et les notices pour PERISCOPE
 */
@Component
@Slf4j
public class NoticeFormatExportMapper {
    private UtilsMapper utilsMapper;

    @Autowired
    public NoticeFormatExportMapper(UtilsMapper utilsMapper) { this.utilsMapper = utilsMapper; }

    /**
     * Convertisseur pour les notices XML vers les notices de visualisation
     */
    @Bean
    public void converterNoticeXML() {

        Converter<NoticeXml, NoticeVisu> myConverter = new Converter<NoticeXml, NoticeVisu>() {

            public NoticeVisu convert(MappingContext<NoticeXml, NoticeVisu> context) {
                NoticeXml source = context.getSource();
                NoticeVisu target = new NoticeVisu();
                int maxPass = 1;
                try {
                    // Champ type de support
                    target.setSupportType(extractSupportType(source.getLeader().substring(6, 7)));
                    // Champs PPN
                    target.setPpn(source.getControlFields().stream().filter(elm -> elm.getTag().equalsIgnoreCase("001")).findFirst().orElseThrow().getValue());

                    for (int currentPass = 1; currentPass < maxPass + 1; currentPass++) {

                        // Champs data fields
                        for (DataField dataField : source.getDataFields()) {
                            // Zone 011
                            if (currentPass == 1 && dataField.getTag().equalsIgnoreCase("011")) {

                                for (SubField subField : dataField.getSubFields()) {
                                    // zone 011-a
                                    if (subField.getCode().equalsIgnoreCase("a")) {
                                        target.setIssn(subField.getValue());
                                    }
                                }
                            }

                            // Zone 100
                            if (currentPass == 1 && dataField.getTag().equalsIgnoreCase("100")) {
                                for (SubField subField : dataField.getSubFields()) {
                                    // zone 100-a
                                    if (subField.getCode().equalsIgnoreCase("a")) {
                                        String value = subField.getValue();

                                        // Extraction de la date de début
                                        try {
                                            PublicationYear year = utilsMapper.buildStartPublicationYear(value);
                                            target.setStartYear(year);
                                        } catch (IllegalPublicationYearException e) {
                                            log.debug("Unable to parse start publication year :" + e.getLocalizedMessage());
                                            target.setStartYear(null);
                                        }

                                        // Extraction de la date de fin
                                        try {
                                            PublicationYear year = utilsMapper.buildEndPublicationYear(value);
                                            target.setEndYear(year);
                                        } catch (IllegalPublicationYearException e) {
                                            log.debug("Unable to parse end publication year :" + e.getLocalizedMessage());
                                            target.setEndYear(null);
                                        }
                                    }
                                }
                            }

                            // Zone 110
                            if (currentPass == 1 && dataField.getTag().equalsIgnoreCase("110")) {
                                for (SubField subField : dataField.getSubFields()) {
                                    // zone 110-a
                                    if (subField.getCode().equalsIgnoreCase("a")) {
                                        target.setContinuousType(extractOnGoingResourceType(subField.getValue()));
                                        target.setFrequency(extractFrequency(subField.getValue().substring(1, 2)));
                                    }
                                }
                            }

                            // Zone 200
                            if (currentPass == 1 && dataField.getTag().equalsIgnoreCase("200")) {

                                for (SubField subField : dataField.getSubFields()) {
                                    // zone 200-a
                                    if (subField.getCode().equalsIgnoreCase("a")) {
                                        if (target.getProperTitle() == null) {
                                            target.setProperTitle(subField.getValue());
                                        }
                                    }

                                    // zone 200-c
                                    if (subField.getCode().equalsIgnoreCase("c")) {
                                        if (target.getTitleFromDifferentAuthor() == null) {
                                            target.setTitleFromDifferentAuthor(subField.getValue());
                                        }
                                    }

                                    // zone 200-d
                                    if (subField.getCode().equalsIgnoreCase("d") && (target.getParallelTitle() == null)) {
                                        target.setParallelTitle(subField.getValue());
                                    }

                                    // zone 200-e
                                    if (subField.getCode().equalsIgnoreCase("e") && (target.getTitleComplement() == null)) {
                                        target.setTitleComplement(subField.getValue());
                                    }

                                    // zone 200-i
                                    if (subField.getCode().equalsIgnoreCase("i") && (target.getSectionTitle() == null)) {
                                        target.setSectionTitle(subField.getValue());
                                    }
                                }
                            }

                            // Zone 210
                            if (currentPass == 1 && dataField.getTag().equalsIgnoreCase("210")) {
                                for (SubField subField : dataField.getSubFields()) {
                                    // zone 210-c
                                    if (subField.getCode().equalsIgnoreCase("c") && (target.getPublisher() == null)) {
                                        target.setPublisher(subField.getValue());
                                    }
                                }
                            }

                            // Zone 530
                            if (currentPass == 1 && dataField.getTag().equalsIgnoreCase("530")) {

                                for (SubField subField : dataField.getSubFields()) {
                                    // zone 530-a
                                    if (subField.getCode().equalsIgnoreCase("a")) {
                                        target.setKeyTitle(subField.getValue());
                                    }

                                    // zone 530-b
                                    if (subField.getCode().equalsIgnoreCase("b")) {
                                        target.setKeyTitleQualifer(subField.getValue());
                                    }
                                }
                            }

                            // Zone 531
                            if (currentPass == 1 && dataField.getTag().equalsIgnoreCase("531")) {

                                for (SubField subField : dataField.getSubFields()) {
                                    if (target.getKeyShortedTitle() == null) {
                                        target.setKeyShortedTitle(subField.getValue());
                                    }
                                }
                            }

                            // Zone 9XX
                            if (dataField.getTag().startsWith("9")) {

                                // On cherche la sous-zone 5 qui contient le EPN
                                SubField specimenIdField = dataField.getSubFields().stream().filter(elm -> elm.getCode().equalsIgnoreCase("5"))
                                        .findAny().orElse(null);

                                if (specimenIdField == null) {
                                    throw new MissingFieldException("Zone " + dataField.getTag() + " doesn't have a subfield code=\"5\"");
                                }

                                String epn = specimenIdField.getValue().split(":")[1];

                                // On récupère l'exemplaire ou on le crée s'il n'existe pas
                                Holding holding = target.getHoldings().stream().filter(elm -> elm.getEpn().equalsIgnoreCase(epn))
                                        .findAny().orElse(null);

                                if (holding == null) {
                                    holding = new Holding(epn);
                                }

                                if (currentPass == 1 && dataField.getTag().equalsIgnoreCase("955")) {
                                    processEtatCollection(holding, dataField);
                                } else if (currentPass == 1 && dataField.getTag().equalsIgnoreCase("959")) {
                                    // On traite les lacunes dans la deuxième passe
                                    processLacunes(holding, dataField);
                                } else if (currentPass == 1) {
                                    // On itère sur les autres sous-zone
                                    for (SubField subField : dataField.getSubFields()) {
                                        if (dataField.getTag().equalsIgnoreCase("930") && (subField.getCode().equalsIgnoreCase("b"))) {
                                            holding.setRcr(subField.getValue());
                                        }
                                    }
                                }
                                target.addHolding(holding);
                            }
                        }
                    }
                    return target;
                } catch (NullPointerException ex) {
                    throw new MappingException(Collections.singletonList(new ErrorMessage("Notice has null field")));
                } catch (IllegalHoldingException ex) {
                    throw new MappingException(Collections.singletonList(new ErrorMessage(ex.getLocalizedMessage())));
                } catch (Exception ex) {
                    throw new MappingException(Collections.singletonList(new ErrorMessage(ex.getLocalizedMessage())));
                }

            }
        };
        utilsMapper.addConverter(myConverter);
    }

    /**
     * Méthode permettant de générer les séquences d'un état de collection contenu dans une 955 du format d'export
     *
     * @param holding : l'objet représentant les états de collection de la notice qui sera alimenté avec les séquences de la 955 parsée
     * @param dataField : la zone 955 à parser
     *
     * @throws IllegalHoldingException si une erreur est détectée dans la 955
     */
    protected void processEtatCollection(Holding holding, DataField dataField) throws IllegalHoldingException {
        Iterator<SubField> subFieldIterator = dataField.getSubFields().iterator();
        String sousZonePrecedente;

        // Prorpiété d'une séquence continue
        Integer startYear = null;
        String startVolume = null;
        String startNumero = null;

        Integer endYear = null;
        String endNumero = null;
        String endVolume = null;

        boolean ouvert = false;

        // Compteurs d'occurence des balises
        //compteur de volumes
        int aCount = 0;
        //compteur de numéro
        int bCount = 0;
        //compteur de date de début / date de fin
        int iCount = 0;

        boolean erreur = false;
        while (subFieldIterator.hasNext()) {
            SubField subField = subFieldIterator.next();
            if (subField.getCode().equalsIgnoreCase("g")) {
                //si on rencontre une $g, les sous zones suivantes correspondent à une séquence de numérotation parallèle (dernière séquence dans la zone)
                //on sort de la boucle
                break;
            }
            switch (subField.getCode()) {
                case "r":
                    holding.setTextEtatCollection(subField.getValue());
                    break;
                case "w":
                    holding.setMentionDeLacune(subField.getValue());
                    break;
                case "z":
                    holding.setNote(subField.getValue());
                    break;
                case "5":
                    break;
                default:
                    if (subField.getValue() == null) {
                        //si la valeur de la sous zone est vide, on est dans le cas d'un intervale ouvert, on sort de la boucle
                        ouvert = true;
                        break;
                    }
                    try {
                        //volume
                        if (subField.getCode().equalsIgnoreCase("a")) {
                            if (aCount == 0) { //Première fois qu'on rencontre la balise
                                startVolume = subField.getValue();
                            } else if (aCount == 1) {
                                endVolume = subField.getValue();
                            }
                            aCount++;
                        }
                        //numéro
                        if (subField.getCode().equalsIgnoreCase("b")) {
                            if (bCount == 0) { //Première fois qu'on rencontre la balise
                                startNumero = subField.getValue();
                            } else if (bCount == 1) {
                                endNumero = subField.getValue();
                            }
                            bCount++;
                        }

                        //annee
                        if (subField.getCode().equalsIgnoreCase("i")) {
                            if (iCount == 0) { //Première fois qu'on rencontre la balise
                                startYear = Integer.parseInt(subField.getValue().trim());
                            } else if (iCount == 1) {
                                endYear = Integer.parseInt(subField.getValue().trim());
                            }
                        }
                    } catch (IllegalDateException | NumberFormatException ex) {
                        holding.addErreur("Erreur epn " + holding.getEpn() + " : syntaxe de date incorrecte : " + subField.getValue());
                        erreur = true;
                    }
            }
            sousZonePrecedente = subField.getCode().toLowerCase(Locale.ROOT);
            if (sousZonePrecedente.equals("i")) {
                iCount++;
            }
        }

        if (iCount >= 0 && startYear != null) {
            // La date de début a été trouvé
            //on ajout la séquence uniquement si elle a une date de début pour gérer le cas ou la 955 n'a que des sous zones de note
            try {
                SequenceContinue sequence = new SequenceContinue(startYear, startVolume, startNumero, ouvert);
                if (iCount >= 2) {
                    // La date de fin a été trouvé
                    sequence.setEndDate(endYear);
                    sequence.setEndNumero(endNumero);
                    sequence.setEndVolume(endVolume);
                }
                if (erreur) {
                    SequenceError sequenceError = new SequenceError(sequence, "Erreur dans la saisie du mois");
                    holding.addSequence(sequenceError);
                } else {
                    holding.addSequence(sequence);
                }

            } catch (IllegalDateException ex) {
                log.error("Impossible de créer la séquence continue : " + ex.getLocalizedMessage());
                if (startYear != null) {
                    // Il ne s'agit pas d'une séquence vide alors remonte l'erreur
                    throw new IllegalHoldingException("Impossible de créer la séquence continue : " + ex.getLocalizedMessage());
                }
            }
        }
    }

    /**
     * Méthode permettant de générer générer les séquences lacunaires d'une 959
     * @param holding : objet réprésentant les états de collection de la notice dans lequel seront ajoutées les séquences lacunaires
     * @param dataField : la zone parsée pour extraire les séquences lacunaires
     * @throws IllegalHoldingException
     */
    void processLacunes(Holding holding, DataField dataField) throws IllegalHoldingException {
        Iterator<SubField> subFieldIterator = dataField.getSubFields().iterator();

        // Prorpiété d'une séquence continue
        Integer startYear = null;
        String volume = null;
        String numero = null;

        boolean error = false;
        String errorMessage = "";
        while (subFieldIterator.hasNext()) {
            SubField subField = subFieldIterator.next();

            switch (subField.getCode()) {
                case "r":
                    holding.setTextLacune(subField.getValue());
                    break;
                case "5":
                    break;
                default:
                    try {
                        if (subField.getCode().equals("0")) {
                            //si on arrive sur une $0, on crée un nouveau bloc
                            if (!error) {
                                SequenceLacune sequence = new SequenceLacune(startYear, volume, numero);
                                holding.addSequence(sequence);
                            }
                        }
                        if (subField.getCode().equalsIgnoreCase("d")) {
                            volume = subField.getValue();
                        }
                        if (subField.getCode().equalsIgnoreCase("e")) {
                            numero = subField.getValue();
                        }
                        if (subField.getCode().equalsIgnoreCase("a")) {
                            startYear = Integer.parseInt(subField.getValue());

                        }
                    } catch (IllegalDateException | NumberFormatException ex) {
                        holding.addErreur("Erreur epn " + holding.getEpn() + " : syntaxe de date incorrecte : " + ex.getLocalizedMessage());
                        error = true;
                    }
            }
        }


        if (error) {
            if (startYear != null) {
                SequenceError sequenceError = new SequenceError(startYear, errorMessage);
                holding.addSequence(sequenceError);
            }
        } else {
            //ajout du dernier bloc qui n'est pas ajouté en début de boucle
            Sequence sequence = new SequenceLacune(startYear, volume, numero);
            holding.addSequence(sequence);
        }
    }

    /**
     * Extrait le type de ressource continue
     *
     * @param continiousType
     * @return String Type de ressource continue
     */
    public String extractOnGoingResourceType(String continiousType) {

        if (continiousType == null) {
            return OnGoingResourceType.X;
        }

        switch (continiousType.substring(0, 1).toLowerCase()) {
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

    protected Period extractFrequency(String value) {
        switch (value.toUpperCase()) {
            case "A":
                // Quotidienne
                return Period.ofDays(1);
            case "B":
                // Bihebdomadaire
                return Period.ofDays(3);
            case "C":
                // Hebdomadaire
                return Period.ofDays(7);
            case "D":
            case "E":
                // Bimensuelle
                // Toutes les deux semaines
                return Period.ofWeeks(2);
            case "F":
                // Mensuelle
                return Period.ofMonths(1);
            case "G":
                // Bimestrielle
                return Period.ofMonths(2);
            case "H":
                // Trimestrielle
                return Period.ofMonths(3);
            case "I":
            case "J":
                // Semestrielle
                // Trois fois par an
                return Period.ofMonths(4);
            default:
                return Period.ZERO;
        }
    }

    public String extractSupportType(String typeSupport) {
        if (typeSupport == null) {
            return SupportType.X;
        }
        switch (typeSupport.toLowerCase()) {
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
