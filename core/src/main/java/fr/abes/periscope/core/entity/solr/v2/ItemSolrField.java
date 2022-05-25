package fr.abes.periscope.core.entity.solr.v2;

/**
 * Représente les champs SolR pour un Exemplaire SolR
 */
public abstract class ItemSolrField {

    public static final String ID = "id";
    public static final String EPN = "zone_930$5";

    //--------------------------------
    // Zone 930
    public static final String PCP = "zone_930$z_t";
    public static final String RCR = "zone_930$b";
    public static final String STATUT = "zone_930$p";
    //--------------------------------

    public static final String PPN_PARENT = "ppn_parent";

}
