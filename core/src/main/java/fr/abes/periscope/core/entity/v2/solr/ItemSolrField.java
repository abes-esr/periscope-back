package fr.abes.periscope.core.entity.v2.solr;

/**
 * Représente les champs SolR pour un Exemplaire SolR
 */
public abstract class ItemSolrField {

    public static final String ID = "id";
    public static final String ID_TYPE = "string";

    public static final String EPN = "930_5";
    public static final String EPN_TYPE = "string";

    //--------------------------------
    // Zone 930
    public static final String PCP = "930_z";
    public static final String PCP_TYPE = "strings";

    public static final String RCR = "930_b";
    public static final String RCR_TYPE = "strings";
    //--------------------------------

}
