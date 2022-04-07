package fr.abes.periscope.web.dto;

import com.fasterxml.jackson.annotation.JsonGetter;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import fr.abes.periscope.core.entity.solr.PublicationYear;
import lombok.Getter;
import lombok.Setter;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Représente les info du'une Notice au format JSON de l'API
 * info à montrer : PPN, ISSN, titre , date de debut et fin, editeur, la ville?, et frequence  periodique
 */
@Getter
@Setter
public class NoticeInfoWebDto {

    @JsonProperty("ppn")
    private String ppn;

    @JsonProperty("issn")
    private String issn;

    @JsonProperty("editeur")
    private String editeur;

    @JsonProperty("titre")
    private String titre;

    @JsonProperty("date_de_debut")
    private String date_de_debut;

    @JsonProperty("date_de_fin")
    private String date_de_fin;

    @JsonProperty("ville")
    private String ville;

    @JsonProperty("frenquence_periodique")
    private String frenquence_periodique;
}
