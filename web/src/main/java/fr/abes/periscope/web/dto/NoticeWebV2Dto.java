package fr.abes.periscope.web.dto;

import com.fasterxml.jackson.annotation.JsonGetter;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import fr.abes.periscope.core.entity.PublicationYear;
import fr.abes.periscope.core.entity.v2.solr.ItemSolr;
import lombok.Getter;
import lombok.Setter;

import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

/**
 * Représente un Notice V2 au format JSON de l'API
 */
@Getter
@Setter
public class NoticeWebV2Dto {

    @JsonProperty("ppn")
    private String ppn;

    @JsonProperty("issn")
    private String issn;

    @JsonProperty("editeur")
    private String editeur;

    @JsonIgnore
    private String titrePropre;

    @JsonIgnore
    private String titreAuteurDifferent;

    @JsonIgnore
    private String titreParallele;

    @JsonIgnore
    private String titreComplement;

    @JsonIgnore
    private String titreSection;

    @JsonIgnore
    private String titreCle;

    @JsonIgnore
    private String titreCleQualifie;

    @JsonIgnore
    private String titreCleCourt;

    @JsonGetter("titre")
    protected String getTitre() {
        String titre = this.titreCle;
        if (titre != null && !titre.isEmpty() && this.titreCleQualifie != null) {
            titre += " " + this.titreCleQualifie;
            return titre;
        }
        if (titre == null || titre.isEmpty()) {
            if (this.titreCleCourt != null && !this.titreCleCourt.isEmpty()) {
                return this.titreCleCourt;
            }

            if (this.titrePropre != null && !this.titrePropre.isEmpty()) {
                return this.titrePropre;
            }

            if (this.titreAuteurDifferent != null && !this.titreAuteurDifferent.isEmpty()) {
                return this.titreAuteurDifferent;
            }

            if (this.titreParallele != null && !this.titreParallele.isEmpty()) {
                return this.titreParallele;
            }

            if (this.titreComplement != null && !this.titreComplement.isEmpty()) {
                return this.titreComplement;
            }
        }
        if (titre != null) {
            //suppression des caractères entourant les mots vides pour l'affichage
            return titre.replace("\u0098", "").replace("\u009c", "");
        }
        return null;
    }

    @JsonProperty("typeDocument")
    private String typeRessourceContinue;

    @JsonProperty("typeSupport")
    private String typeSupport;

    @JsonProperty("langue")
    private String langue;

    @JsonProperty("pays")
    private String pays;

    @JsonIgnore
    private PublicationYear startYear;
    @JsonIgnore
    private PublicationYear endYear;

    @JsonGetter("date_debut")
    protected String getStartYear() {
        if (startYear == null) {
            return null;
        } else {
            return startYear.getYear();
        }
    }

    @JsonGetter("date_debut_indice")
    protected Integer getStartYearConfidenceIndex() {
        if (startYear == null) {
            return null;
        } else {
            return startYear.getConfidenceIndex();
        }
    }

    @JsonGetter("date_fin")
    protected String getEndYear() {
        if (endYear == null) {
            return null;
        } else {
            return endYear.getYear();
        }
    }

    @JsonGetter("date_fin_indice")
    protected Integer getEndYearConfidenceIndex() {
        if (endYear == null) {
            return null;
        } else {
            return endYear.getConfidenceIndex();
        }
    }

    @JsonProperty("lien_mirabel")
    private String mirabelURL;

    @JsonProperty("nb_location")
    private Integer nbLocation;

    @JsonProperty("lien_sudoc")
    private String sudocURL;

    @JsonProperty("exemplaires")
    private Set<ItemWebDto> items = new HashSet<>();

    // Support avec la V1
    @JsonGetter("pcpList")
    protected HashSet<String> getPcpList() {
        HashSet<String> list = new HashSet<>();

        Iterator<ItemWebDto> itemIterator = getItems().iterator();
        while (itemIterator.hasNext()) {
            ItemWebDto item = itemIterator.next();
            if (item.getPcp() != null)
                list.add(item.getPcp());
        }

        return list;
    }

    @JsonGetter("rcrList")
    protected HashSet<String> getRcrList() {
        HashSet<String> list = new HashSet<>();

        Iterator<ItemWebDto> itemIterator = getItems().iterator();
        while (itemIterator.hasNext()) {
            ItemWebDto item = itemIterator.next();
            list.add(item.getRcr());
        }

        return list;
    }

    public void addItem(ItemWebDto item) {
        this.items.add(item);
    }

}
