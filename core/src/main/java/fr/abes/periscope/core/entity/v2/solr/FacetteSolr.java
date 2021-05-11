package fr.abes.periscope.core.entity.v2.solr;

import lombok.Data;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

@Data
public class FacetteSolr {
    private String zone;
    private List<Map<String, Integer>> valeurs;

    public FacetteSolr(String name) {
        this.zone = name;
        this.valeurs = new ArrayList<>();
    }
}
