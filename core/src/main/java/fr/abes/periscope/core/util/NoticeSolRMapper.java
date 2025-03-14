package fr.abes.periscope.core.util;

import fr.abes.periscope.core.entity.solr.Item;
import fr.abes.periscope.core.entity.solr.Notice;
import fr.abes.periscope.core.entity.solr.OnGoingResourceType;
import fr.abes.periscope.core.entity.solr.PublicationYear;
import fr.abes.periscope.core.entity.solr.ItemSolr;
import fr.abes.periscope.core.entity.solr.NoticeSolr;
import fr.abes.periscope.core.entity.xml.DataField;
import fr.abes.periscope.core.entity.xml.NoticeXml;
import fr.abes.periscope.core.entity.xml.SubField;
import fr.abes.periscope.core.exception.IllegalPublicationYearException;
import fr.abes.periscope.core.exception.MissingFieldException;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.Converter;
import org.modelmapper.MappingException;
import org.modelmapper.spi.ErrorMessage;
import org.modelmapper.spi.MappingContext;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.stereotype.Component;

import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

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
     * Convertisseur pour les notices SolR V2 vers les notices PERISCOPE
     */
    @Bean
    public void converterNoticeV2Solr() {

        Converter<NoticeSolr, Notice> myConverter = new Converter<NoticeSolr, Notice>() {
            @Override
            public Notice convert(MappingContext<NoticeSolr, Notice> context) {
                NoticeSolr source = context.getSource();
                Notice target = new Notice();

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
                    target.setContinuousType((source.getContinuousType() == null ? "Non renseigné" : source.getContinuousType()));
                    target.setSupportType(source.getSupportType());
                    target.setLanguage(source.getLanguageForDisplay());
                    target.setCountry(source.getCountryForDisplay());

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
            case "d":
                return OnGoingResourceType.D;
            case "e":
                return OnGoingResourceType.E;
            case "f":
                return OnGoingResourceType.F;
            case "g":
                return OnGoingResourceType.G;
            case "h":
                return OnGoingResourceType.H;
            case "i":
                return OnGoingResourceType.I;
            case "j":
                return OnGoingResourceType.J;
            case "m":
                return OnGoingResourceType.M;
            case "n":
                return OnGoingResourceType.N;
            case "z":
                return OnGoingResourceType.Z;
            default:
                return OnGoingResourceType.X;
        }
    }

    /**
     * Convertisseur pour les notices XML vers les notices SolR avec des exemplaires
     */
    @Bean
    public void converterNoticeXMLToNoticeSolr() {

        Converter<NoticeXml, NoticeSolr> myConverter = new Converter<NoticeXml, NoticeSolr>() {

            public NoticeSolr convert(MappingContext<NoticeXml, NoticeSolr> context) {
                NoticeXml source = context.getSource();
                NoticeSolr target = new NoticeSolr();
                try {
                    boolean deleteFlag = source.getLeader().substring(5, 6).equalsIgnoreCase("d");
                    target.setToDelete(deleteFlag);
                    // Champ type de support
                    target.setSupportType(utilsMapper.extractSupportType(source.getLeader().substring(6,7)));
                    // ID
                    target.setId(source.getControlFields().stream().filter(elm -> elm.getTag().equalsIgnoreCase("001")).findFirst().get().getValue());

                    // Champs PPN
                    target.setPpn(source.getControlFields().stream().filter(elm -> elm.getTag().equalsIgnoreCase("001")).findFirst().get().getValue());

                    // Champs data fields
                    for (DataField dataField : source.getDataFields()) {
                        // Zone 011
                        if (dataField.getTag().equalsIgnoreCase("011")) {

                            for (SubField subField : dataField.getSubFields()) {
                                // zone 011-a
                                if (subField.getCode().equals("a")) {
                                    target.setIssn(subField.getValue());
                                }
                            }
                        }

                        //zone 033
                        if (dataField.getTag().equalsIgnoreCase("033")) {
                            for (SubField subField : dataField.getSubFields()) {
                                if (subField.getCode().equals("a")) {
                                    target.addExternalUrl(subField.getValue());
                                }
                            }
                        }
                        // Zone 100
                        if (dataField.getTag().equalsIgnoreCase("100")) {
                            for (SubField subField : dataField.getSubFields()) {
                                // zone 100-a
                                if (subField.getCode().equals("a")) {
                                    String value = subField.getValue();
                                    target.setProcessingGlobalData(value);

                                    // Extraction de la date de début
                                    try {
                                        PublicationYear year = utilsMapper.buildStartPublicationYear(value);
                                        target.setStartYear(year.getYear());
                                        target.setStartYearConfidenceIndex(year.getConfidenceIndex());
                                    } catch (IllegalPublicationYearException e) {
                                        log.debug("Unable to parse start publication year :" + e.getLocalizedMessage());
                                        target.setStartYear(null);
                                    }

                                    // Extraction de la date de fin
                                    try {
                                        PublicationYear year = utilsMapper.buildEndPublicationYear(value);
                                        target.setEndYear(year.getYear());
                                        target.setEndYearConfidenceIndex(year.getConfidenceIndex());
                                    } catch (IllegalPublicationYearException e) {
                                        log.debug("Unable to parse end publication year :" + e.getLocalizedMessage());
                                        target.setEndYear(null);
                                    }
                                }
                            }
                        }

                        // Zone 101
                        if (dataField.getTag().equalsIgnoreCase("101")) {
                            for (SubField subField : dataField.getSubFields()) {
                                // zone 101-a
                                if (subField.getCode().equals("a")) {
                                    if (target.getLanguageForDisplay() == null) {
                                        target.setLanguageForDisplay(subField.getValue());
                                    }
                                    target.addLanguage(subField.getValue());
                                }
                            }
                        }

                        // Zone 102
                        if (dataField.getTag().equalsIgnoreCase("102")) {
                            for (SubField subField : dataField.getSubFields()) {
                                // zone 102-a
                                if (subField.getCode().equals("a")) {
                                    if (target.getCountryForDisplay() == null) {
                                        target.setCountryForDisplay(subField.getValue());
                                    }
                                    target.addCountry(subField.getValue());
                                }
                            }
                        }

                        // Zone 110
                        if (dataField.getTag().equalsIgnoreCase("110")) {
                            for (SubField subField : dataField.getSubFields()) {
                                // zone 110-a
                                if (subField.getCode().equals("a")) {
                                    target.setContinuousType(extractOnGoingResourceType(subField.getValue()));
                                }
                            }
                        }

                        // Zone 200
                        if (dataField.getTag().equalsIgnoreCase("200")) {

                            for (SubField subField : dataField.getSubFields()) {
                                // zone 200-a
                                if (subField.getCode().equals("a")) {
                                    if (target.getProperTitleForDisplay() == null) {
                                        target.setProperTitleForDisplay(subField.getValue());
                                    }
                                    target.addProperTitle(subField.getValue());
                                }

                                // zone 200-c
                                if (subField.getCode().equals("c")) {
                                    if (target.getTitleFromDifferentAuthorForDisplay() == null) {
                                        target.setTitleFromDifferentAuthorForDisplay(subField.getValue());
                                    }
                                    target.addTitleFromDifferentAuthor(subField.getValue());
                                }

                                // zone 200-d
                                if (subField.getCode().equals("d")) {
                                    if (target.getParallelTitleForDisplay() == null) {
                                        target.setParallelTitleForDisplay(subField.getValue());
                                    }
                                    target.addParallelTitle(subField.getValue());
                                }

                                // zone 200-e
                                if (subField.getCode().equals("e")) {
                                    if (target.getTitleComplementForDisplay() == null) {
                                        target.setTitleComplementForDisplay(subField.getValue());
                                    }
                                    target.addTitleComplement(subField.getValue());
                                }

                                // zone 200-i
                                if (subField.getCode().equals("i")) {
                                    if (target.getSectionTitleForDisplay() == null) {
                                        target.setSectionTitleForDisplay(subField.getValue());
                                    }
                                    target.addSectionTitle(subField.getValue());
                                }
                            }
                        }

                        // Zone 210
                        if (dataField.getTag().equalsIgnoreCase("210")) {

                            for (SubField subField : dataField.getSubFields()) {
                                // zone 210-c
                                if (subField.getCode().equals("c")) {
                                    if (target.getEditorForDisplay() == null) {
                                        target.setEditorForDisplay(subField.getValue());
                                    }
                                    target.addEditor(subField.getValue());
                                }
                            }
                        }
                        // todo verifier si pas de 210-c sur 214-c

                        // Zone 214
                        if (dataField.getTag().equalsIgnoreCase("214")) {

                            for (SubField subField : dataField.getSubFields()) {
                                // zone 214-c
                                if (subField.getCode().equals("c")) {
                                    if (target.getEditorForDisplay() == null) {
                                        target.setEditorForDisplay(subField.getValue());
                                    }
                                    target.addEditor(subField.getValue());
                                }
                            }
                        }

                        // Zone 530
                        if (dataField.getTag().equalsIgnoreCase("530")) {

                            for (SubField subField : dataField.getSubFields()) {
                                // zone 530-a
                                if (subField.getCode().equals("a")) {
                                    target.setKeyTitle(subField.getValue());
                                }

                                // zone 530-b
                                if (subField.getCode().equals("b")) {
                                    target.setKeyTitleQualifer(subField.getValue());
                                }
                            }
                        }

                        // Zone 531
                        if (dataField.getTag().equalsIgnoreCase("531")) {

                            for (SubField subField : dataField.getSubFields()) {
                                if (target.getKeyShortedTitleForDisplay() == null) {
                                    target.setKeyShortedTitleForDisplay(subField.getValue());
                                }
                                // zone 531-a
                                if (subField.getCode().equals("a")) {
                                    target.addKeyShortedTitle(subField.getValue());
                                }
                            }
                        }

                        target.setTriTitre();
                        // Zone 9XX
                        if (dataField.getTag().startsWith("9")) {

                            // On cherche la sous-zone 5 qui contient le EPN
                            String epn = NoticeFormatExportMapper.findEpnInSubfield5(dataField);

                            // On récupère l'exemplaire ou on le crée s'il n'existe pas
                            ItemSolr itemSolr = target.getItems().stream().filter(elm -> elm.getId().equalsIgnoreCase(epn))
                                    .findAny().orElse(null);

                            if (itemSolr == null) {
                                itemSolr = new ItemSolr(target.getPpn(), epn);
                            }

                            // On itère sur les autres sous-zone
                            for (SubField subField : dataField.getSubFields()) {
                                if (dataField.getTag().equalsIgnoreCase("930")) {
                                    if (subField.getCode().equals("b")) {
                                        itemSolr.setRcr(subField.getValue());
                                        target.addRcr(subField.getValue());
                                    }
                                    if (subField.getCode().equals("z")) {
                                        itemSolr.addPcp(subField.getValue());
                                        target.addPcp(subField.getValue());
                                    }
                                    if (subField.getCode().equals("p")) {
                                        if (subField.getValue().equalsIgnoreCase("Membre du plan de conservation")) {
                                            itemSolr.setStatutBibliotheque("PA");
                                            target.addStatut("PA");
                                        } else {
                                            itemSolr.setStatutBibliotheque("PC");
                                            target.addStatut("PC");
                                        }
                                    }

                                }
                            }

                            target.addItem(itemSolr);
                        }
                    }
                    if (!target.getPcpList().isEmpty() && target.getStatutList().isEmpty()) {
                        target.addStatut("Orphelin");
                    }
                    target.setNbLocation(target.getRcrList().size());
                    target.setNbPcp(target.getPcpList().size());

                    return target;

                } catch (NullPointerException ex) {
                    throw new MappingException(Arrays.asList(new ErrorMessage("NoticeSolr has null field")));
                } catch (Exception ex) {
                    throw new MappingException(Arrays.asList(new ErrorMessage("PPN" + source.getPpn() + " : " + ex.getMessage())));
                }

            }
        };
        utilsMapper.addConverter(myConverter);
    }




}
