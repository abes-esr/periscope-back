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

    @JsonProperty("titre")
    private String titre;

    @JsonProperty("datePublication")
    private String datePublication;

    @JsonProperty("editeur")
    private String editeur;

    @JsonProperty("ville")
    private String ville;

    @JsonProperty("typeSupport")
    private String typeSupport;

    @JsonProperty("periodicite")
    private String periodicite;
}
