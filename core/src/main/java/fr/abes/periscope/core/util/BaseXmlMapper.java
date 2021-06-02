package fr.abes.periscope.core.util;

import fr.abes.periscope.core.entity.EnumMonth;
import fr.abes.periscope.core.entity.OnGoingResourceType;
import fr.abes.periscope.core.entity.PublicationYear;
import fr.abes.periscope.core.entity.SupportType;
import fr.abes.periscope.core.entity.visualisation.*;
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

import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Convertisseurs entre les notices issues de la base XML et les notices pour PERISCOPE
 */
@Component
@Slf4j
public class BaseXmlMapper {
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
     * Convertisseur pour les notices XML vers les notices SolR avec des exemplaires
     */
    @Bean
    public void converterNoticeXML() {

        Converter<NoticeXml, NoticeVisu> myConverter = new Converter<NoticeXml, NoticeVisu>() {

            public NoticeVisu convert(MappingContext<NoticeXml, NoticeVisu> context) {
                NoticeXml source = context.getSource();
                NoticeVisu target = new NoticeVisu();
                try {
                    // Champ type de support
                    target.setSupportType(extractSupportType(source.getLeader().substring(6, 7)));
                    // Champs PPN
                    target.setPpn(source.getControlFields().stream().filter(elm -> elm.getTag().equalsIgnoreCase("001")).findFirst().orElseThrow().getValue());

                    // Champs data fields
                    Iterator<DataField> iterator = source.getDataFields().iterator();
                    while (iterator.hasNext()) {
                        DataField dataField = iterator.next();

                        // Zone 011
                        if (dataField.getTag().equalsIgnoreCase("011")) {

                            Iterator<SubField> subFieldIterator = dataField.getSubFields().iterator();
                            while (subFieldIterator.hasNext()) {
                                SubField subField = subFieldIterator.next();
                                // zone 011-a
                                if (subField.getCode().equalsIgnoreCase("a")) {
                                    target.setIssn(subField.getValue());
                                }
                            }
                        }

                        // Zone 100
                        if (dataField.getTag().equalsIgnoreCase("100")) {
                            Iterator<SubField> subFieldIterator = dataField.getSubFields().iterator();
                            while (subFieldIterator.hasNext()) {
                                SubField subField = subFieldIterator.next();

                                // zone 100-a
                                if (subField.getCode().equalsIgnoreCase("a")) {
                                    String value = subField.getValue();

                                    // Extraction de la date de début
                                    try {
                                        PublicationYear year = buildStartPublicationYear(value);
                                        target.setStartYear(year);
                                    } catch (IllegalPublicationYearException e) {
                                        log.debug("Unable to parse start publication year :" + e.getLocalizedMessage());
                                        target.setStartYear(null);
                                    }

                                    // Extraction de la date de fin
                                    try {
                                        PublicationYear year = buildEndPublicationYear(value);
                                        target.setEndYear(year);
                                    } catch (IllegalPublicationYearException e) {
                                        log.debug("Unable to parse end publication year :" + e.getLocalizedMessage());
                                        target.setEndYear(null);
                                    }
                                }
                            }
                        }

                        // Zone 110
                        if (dataField.getTag().equalsIgnoreCase("110")) {
                            Iterator<SubField> subFieldIterator = dataField.getSubFields().iterator();
                            while (subFieldIterator.hasNext()) {
                                SubField subField = subFieldIterator.next();

                                // zone 110-a
                                if (subField.getCode().equalsIgnoreCase("a")) {
                                    target.setContinuousType(extractOnGoingResourceType(subField.getValue()));
                                }
                            }
                        }

                        // Zone 200
                        if (dataField.getTag().equalsIgnoreCase("200")) {

                            Iterator<SubField> subFieldIterator = dataField.getSubFields().iterator();
                            while (subFieldIterator.hasNext()) {
                                SubField subField = subFieldIterator.next();

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
                                if (subField.getCode().equalsIgnoreCase("d")) {
                                    if (target.getParallelTitle() == null) {
                                        target.setParallelTitle(subField.getValue());
                                    }
                                }

                                // zone 200-e
                                if (subField.getCode().equalsIgnoreCase("e")) {
                                    if (target.getTitleComplement() == null) {
                                        target.setTitleComplement(subField.getValue());
                                    }
                                }

                                // zone 200-i
                                if (subField.getCode().equalsIgnoreCase("i")) {
                                    if (target.getSectionTitle() == null) {
                                        target.setSectionTitle(subField.getValue());
                                    }
                                }
                            }
                        }

                        // Zone 210
                        if (dataField.getTag().equalsIgnoreCase("210")) {

                            Iterator<SubField> subFieldIterator = dataField.getSubFields().iterator();
                            while (subFieldIterator.hasNext()) {
                                SubField subField = subFieldIterator.next();

                                // zone 210-c
                                if (subField.getCode().equalsIgnoreCase("c")) {
                                    if (target.getEditor() == null) {
                                        target.setEditor(subField.getValue());
                                    }
                                }
                            }
                        }

                        // Zone 530
                        if (dataField.getTag().equalsIgnoreCase("530")) {

                            Iterator<SubField> subFieldIterator = dataField.getSubFields().iterator();
                            while (subFieldIterator.hasNext()) {
                                SubField subField = subFieldIterator.next();

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
                        if (dataField.getTag().equalsIgnoreCase("531")) {

                            Iterator<SubField> subFieldIterator = dataField.getSubFields().iterator();
                            while (subFieldIterator.hasNext()) {
                                SubField subField = subFieldIterator.next();
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
                            Holding holding = target.getHoldings().stream().filter(elm -> elm.getId().equalsIgnoreCase(epn))
                                    .findAny().orElse(null);

                            if (holding == null) {
                                holding = new Holding(epn);
                            }
                            try {
                                if (dataField.getTag().equalsIgnoreCase("955")) {
                                    holding.addSequence(genererEtatCollection(dataField));
                                }

                                if (dataField.getTag().equalsIgnoreCase("959")) {
                                    holding.setLacune(genererLacunes(dataField));
                                }
                            } catch (IllegalHoldingException ex) {
                                holding.addErreur("Erreur sur état de collection epn " + epn + " : " + ex.getMessage());
                            }
                            // On itère sur les autres sous-zone
                            Iterator<SubField> subFieldIterator = dataField.getSubFields().iterator();
                            while (subFieldIterator.hasNext()) {
                                SubField subField = subFieldIterator.next();

                                if (dataField.getTag().equalsIgnoreCase("930")) {
                                    if (subField.getCode().equalsIgnoreCase("b")) {
                                        holding.setRcr(subField.getValue());
                                    }
                                }
                            }
                            target.addHolding(holding);
                        }
                    }


                    return target;

                } catch (NullPointerException ex) {
                    throw new MappingException(Arrays.asList(new ErrorMessage("NoticeSolr has null field")));
                } catch (Exception ex) {
                    throw new MappingException(Arrays.asList(new ErrorMessage(ex.getMessage())));
                }

            }
        };
        modelMapper.addConverter(myConverter);
    }


    Sequence genererEtatCollection(DataField dataField) {
        Iterator<SubField> subFieldIterator = dataField.getSubFields().iterator();
        String sousZonePrecedente = "i";
        Bloc blocDates = new BlocDebut();
        Sequence sequence = new Sequence();
        while (subFieldIterator.hasNext()) {
            SubField subField = subFieldIterator.next();
            if (subField.getCode().equalsIgnoreCase("g")) {
                //si on rencontre une $g, les sous zones suivantes correspondent à une séquence de numérotation parallèle (dernière séquence dans la zone)
                //on sort de la boucle
                break;
            }
            switch (subField.getCode()) {
                case "r":
                    sequence.setTexteEtatCollectionZone(subField.getValue());
                    break;
                case "w":
                    sequence.setMentionDeLacune(subField.getValue());
                    break;
                case "z":
                    sequence.setNote(subField.getValue());
                    break;
                case "5":
                    break;
                default:
                    if (sousZonePrecedente.equals("i")) {
                        //si la sous zone précédente est une $i, on est dans un nouveau bloc de numérotation
                        if (sequence.getBlocDebut() != null) {
                            //si le bloc de début existe, on crée un bloc de fin
                            blocDates = new BlocFin();
                            sequence.setBlocFin((BlocFin) blocDates);
                        } else {
                            blocDates = new BlocDebut();
                            sequence.setBlocDebut((BlocDebut) blocDates);
                        }
                    }
                    if (subField.getCode().equalsIgnoreCase("a")) {
                        blocDates.setVolume(subField.getValue());
                        sousZonePrecedente = "a";
                    }
                    if (subField.getCode().equalsIgnoreCase("b")) {
                        blocDates.setNumero(subField.getValue());
                        sousZonePrecedente = "b";
                    }
                    if (subField.getCode().equalsIgnoreCase("j")) {
                        if (EnumUtils.isValidEnum(EnumMonth.class, subField.getValue())) {
                            blocDates.setMois(EnumUtils.getEnum(EnumMonth.class, subField.getValue()));
                        } else {
                            if (blocDates instanceof BlocDebut)
                                throw new IllegalHoldingException("Erreur dans la zone 955 : valeur non autorisée en $c");
                            else
                                throw new IllegalHoldingException("Erreur dans la zone 955 : valeur non autorisée en $m");
                        }
                        sousZonePrecedente = "j";
                    }
                    if (subField.getCode().equalsIgnoreCase("k")) {
                        blocDates.setJour(Integer.parseInt(subField.getValue()));
                        sousZonePrecedente = "k";
                    }
                    if (subField.getCode().equalsIgnoreCase("i")) {
                        blocDates.setAnnee(Integer.parseInt(subField.getValue()));
                        sousZonePrecedente = "i";
                    }
            }
        }
        return sequence;
    }

    Lacune genererLacunes(DataField dataField) {
        Lacune lacune = new Lacune();

        Iterator<SubField> subFieldIterator = dataField.getSubFields().iterator();
        BlocDebut blocDates = new BlocDebut();
        while (subFieldIterator.hasNext()) {
            SubField subField = subFieldIterator.next();
            switch (subField.getCode()) {
                case "r":
                    lacune.setCommentaire(subField.getValue());
                    break;
                case "5":
                    break;
                default:
                    if (subField.getCode().equals("0")) {
                        //si on arrive sur une $0, on crée un nouveau bloc
                        lacune.addBloc(blocDates);
                        blocDates = new BlocDebut();
                    }
                    if (subField.getCode().equalsIgnoreCase("d")) {
                        blocDates.setVolume(subField.getValue());
                    }
                    if (subField.getCode().equalsIgnoreCase("e")) {
                        blocDates.setNumero(subField.getValue());
                    }
                    if (subField.getCode().equalsIgnoreCase("c")) {
                        if (EnumUtils.isValidEnum(EnumMonth.class, subField.getValue())) {
                            blocDates.setMois(EnumUtils.getEnum(EnumMonth.class, subField.getValue()));
                        } else {
                            throw new IllegalHoldingException("Erreur dans la zone 959 : valeur non autorisée en $c");
                        }
                    }
                    if (subField.getCode().equalsIgnoreCase("b")) {
                        blocDates.setJour(Integer.parseInt(subField.getValue()));
                    }
                    if (subField.getCode().equalsIgnoreCase("a")) {
                        blocDates.setAnnee(Integer.parseInt(subField.getValue()));
                    }
            }
        }
        if (blocDates != null) {
            lacune.addBloc(blocDates);
        }
        return lacune;
    }

    /**
     * Extrait l'année de début de publication
     *
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
     *
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
     *
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
     *
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
     * Extrait le type de ressource continue
     *
     * @param continiousType
     * @return String Type de ressource continue
     */
    public String extractOnGoingResourceType(String continiousType) {

        if (continiousType == null) {
            return OnGoingResourceType.X;
        }

        switch (continiousType.substring(0, 1)) {
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
