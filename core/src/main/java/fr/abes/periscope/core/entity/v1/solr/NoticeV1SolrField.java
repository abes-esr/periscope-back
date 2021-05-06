package fr.abes.periscope.core.entity.v1.solr;

/**
 * Représente les champs SolR pour une Notice SolR
 */
@Deprecated
public abstract class NoticeV1SolrField {

    public static final String PCP_S = "930-z_s";
    public static final String PCP_T = "930-z_t";
    public static final String RCR_S = "930-b_s";
    public static final String RCR_T = "930-b_t";
    public static final String PPN = "001_s";
    public static final String ISSN = "011-a_z";
    public static final String ISSN_T = "011-a_t";
    public static final String PCP_LIST = "930-z_t";
    public static final String RCR_LIST = "930-b_t";
    public static final String EDITOR = "210-c_z";
    public static final String EDITOR_T = "210-c_t";
    public static final String PROCESSING_GLOBAL_DATA = "100-a_t";
    public static final String KEY_TITLE = "530-a_z";
    public static final String KEY_TITLE_T = "530-a_t";
    public static final String KEY_SHORTED_TITLE = "531-a_z";
    public static final String KEY_SHORTED_TITLE_T = "531-a_t";
    public static final String PROPER_TITLE = "200-a_z";
    public static final String PROPER_TITLE_T = "200-a_t";
    public static final String TITLE_FROM_DIFFERENT_AUTHOR = "200-c_z";
    public static final String TITLE_FROM_DIFFERENT_AUTHOR_T = "200-c_t";
    public static final String PARALLEL_TITLE = "200-d_z";
    public static final String PARALLEL_TITLE_T = "200-d_t";
    public static final String TITLE_COMPLEMENT = "200-e_z";
    public static final String TITLE_COMPLEMENT_T = "200-e_t";
    public static final String SECTION_TITLE = "200-i_z";
    public static final String SECTION_TITLE_T = "200-i_t";
    public static final String KEY_TITLE_QUALIFIER = "530-b_z";
    public static final String CONTINIOUS_TYPE = "110-a_z";
    public static final String COUNTRY_Z = "102-a_z";
    public static final String COUNTRY_S = "102-a_s";
    public static final String COUNTRY_T = "102-a_t";
    public static final String LANGUAGE = "101-a_t";
    public static final String EXTERNAL_URLS_S = "033-a_s";
    public static final String NB_LOC = "NbLocs_i";
}
