package fr.abes.periscope.core.repository.solr;

/**
 * Représente les champs SolR pour une Notice SolR
 */
public interface NoticeField {

    String PCP_S = "930-z_s";
    String PCP_T = "930-z_t";
    String RCR_S = "930-b_s";
    String RCR_T = "930-b_t";
    String PPN = "001_s";
    String ISSN = "011-a_z";
    String ISSN_T = "011-a_t";
    String PCP_LIST = "930-z_t";
    String RCR_LIST = "930-b_t";
    String EDITOR = "210-c_z";
    String PROCESSING_GLOBAL_DATA = "100-a_t";
    String KEY_TITLE = "530-a_z";
    String KEY_TITLE_T = "530-a_t";
    String KEY_SHORTED_TITLE = "531-a_z";
    String KEY_SHORTED_TITLE_T = "531-a_t";
    String PROPER_TITLE = "200-a_z";
    String PROPER_TITLE_T = "200-a_t";
    String TITLE_FROM_DIFFERENT_AUTHOR = "200-c_z";
    String TITLE_FROM_DIFFERENT_AUTHOR_T = "200-c_t";
    String PARALLEL_TITLE = "200-d_z";
    String PARALLEL_TITLE_T = "200-d_t";
    String TITLE_COMPLEMENT = "200-e_z";
    String TITLE_COMPLEMENT_T = "200-e_t";
    String SECTION_TITLE = "200-i_z";
    String SECTION_TITLE_T = "200-i_t";
    String KEY_TITLE_QUALIFIER = "530-b_z";
    String CONTINIOUS_TYPE = "110-a_z";

}
