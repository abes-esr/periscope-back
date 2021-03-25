package fr.abes.periscope.core.entity.v2.solr;

public interface NoticeV2SolrField {

    String ID = "id";
    String ID_TYPE = "string";

    String TITLE_TYPE = "type_notice";
    String TITLE_TYPE_TYPE = "string";

    String PPN = "001";
    String PPN_TYPE = "string";

    //--------------------------------
    // leader
    String L5 = "L5";
    String L5_TYPE = "strings";

    String L6 = "L6";
    String L6_TYPE = "strings";

    String L7 = "L7";
    String L7_TYPE = "strings";

    String L8 = "L8";
    String L8_TYPE = "strings";

    String L9 = "L9";
    String L9_TYPE = "strings";
    //--------------------------------


    //--------------------------------
    // Zone < 10
    String ZONE_001 = "001";
    String ZONE_001_TYPE = "string";

    String ZONE_002 = "002";
    String ZONE_002_TYPE = "string";

    String ZONE_003 = "003";
    String ZONE_003_TYPE = "string";

    String ZONE_004 = "004";
    String ZONE_004_TYPE = "string";

    String ZONE_005 = "005";
    String ZONE_005_TYPE = "string";

    String ZONE_006 = "006";
    String ZONE_006_TYPE = "string";

    String ZONE_007 = "007";
    String ZONE_007_TYPE = "string";

    String ZONE_008 = "008";
    String ZONE_008_TYPE = "string";

    String ZONE_009 = "009";
    String ZONE_009_TYPE = "string";
    //--------------------------------

    //--------------------------------
    // Zone 011
    String ISSN = "011_a";
    String ISSN_TYPE = "string";

    String ZONE_011_F = "011_f";
    String ZONE_011_F_TYPE = "string";
    //--------------------------------

    //--------------------------------
    // Zone 033
    String EXTERNAL_URLS = "033_a";
    String EXTERNAL_URLS_TYPE = "string";

    String ZONE_033_2 = "033_2";
    String ZONE_033_2_TYPE = "string";

    String ZONE_033_D = "033_d";
    String ZONE_033_D_TYPE = "string";
    //--------------------------------

    //--------------------------------
    // Zone 035
    String ZONE_035_A = "035_a";
    String ZONE_035_A_TYPE = "string";
    //--------------------------------

    //--------------------------------
    // Zone 100
    String PROCESSING_GLOBAL_DATA = "100_a";
    String PROCESSING_GLOBAL_DATA_TYPE = "string";
    //--------------------------------

    //--------------------------------
    // Zone 101
    String LANGUAGE = "101_a";
    String LANGUAGE_TYPE = "string";
    //--------------------------------

    //--------------------------------
    // Zone 102
    String COUNTRY = "102_a";
    String COUNTRY_TYPE = "string";
    //--------------------------------

    //--------------------------------
    // Zone 105
    String ZONE_105_A = "105_a";
    String ZONE_105_A_TYPE = "string";

    //--------------------------------
    // Zone 106
    String ZONE_106_A = "106_a";
    String ZONE_106_A_TYPE = "string";
    //--------------------------------

    //--------------------------------
    // Zone 110
    String CONTINIOUS_TYPE = "110_a";
    String CONTINIOUS_TYPE_TYPE = "string";
    //--------------------------------

    //--------------------------------
    // Zone 181
    String ZONE_181_6 = "181_6";
    String ZONE_181_6_TYPE = "string";

    String ZONE_181_C = "181_c";
    String ZONE_181_C_TYPE = "string";

    String ZONE_181_2 = "181_2";
    String ZONE_181_2_TYPE = "string";
    //--------------------------------

    //--------------------------------
    // Zone 182
    String ZONE_182_6 = "182_6";
    String ZONE_182_6_TYPE = "string";

    String ZONE_182_C = "182_c";
    String ZONE_182_C_TYPE = "string";

    String ZONE_182_2 = "182_2";
    String ZONE_182_2_TYPE = "string";
    //--------------------------------

    //--------------------------------
    // Zone 200
    String PROPER_TITLE = "200_a";
    String PROPER_TITLE_TYPE = "text_fr";

    String ZONE_200_B = "200_b";
    String ZONE_200_B_TYPE = "string";

    String TITLE_FROM_DIFFERENT_AUTHOR = "200_c";
    String TITLE_FROM_DIFFERENT_AUTHOR_TYPE = "text_fr";

    String PARALLEL_TITLE = "200_d";
    String PARALLEL_TITLE_TYPE = "text_fr";

    String TITLE_COMPLEMENT = "200_e";
    String TITLE_COMPLEMENT_TYPE = "text_fr";

    String ZONE_200_F = "200_f";
    String ZONE_200_F_TYPE = "string";

    String ZONE_200_G = "200_g";
    String ZONE_200_G_TYPE = "string";

    String SECTION_TITLE = "200_i";
    String SECTION_TITLE_TYPE = "string";
    //--------------------------------

    //--------------------------------
    // Zone 207
    String ZONE_207_A = "207_a";
    String ZONE_207_A_TYPE = "string";
    //--------------------------------

    //--------------------------------
    // Zone 210
    String ZONE_210_A = "210_a";
    String ZONE_210_A_TYPE = "string";

    String EDITOR = "210_c";
    String EDITOR_TYPE = "text_fr";
    //--------------------------------

    //--------------------------------
    // Zone 530
    String KEY_TITLE = "530_a";
    String KEY_TITLE_TYPE = "text_fr";

    String KEY_TITLE_QUALIFIER = "530_b";
    String KEY_TITLE_QUALIFIER_TYPE = "text_fr";
    //--------------------------------

    //--------------------------------
    // Zone 531
    String KEY_SHORTED_TITLE = "531_a";
    String KEY_SHORTED_TITLE_TYPE = "text_fr";
    //--------------------------------

    //--------------------------------
    // Zone 930
    String PCP_LIST = "930_z";
    String PCP_LIST_TYPE = "strings";

    String RCR_LIST = "930_b";
    String RCR_LIST_TYPE = "strings";
    //--------------------------------

    //--------------------------------
    // Champs supplémentaire
    @Deprecated
    String DATE_AT = "dateetat_dt";
    @Deprecated
    String DATE_AT_TYPE = "pdate";
    @Deprecated
    String DATE_INDEX = "dateindex_dt";
    @Deprecated
    String DATE_INDEX_TYPE = "pdate";

    String NB_LOC = "NbLocs";
    String NB_LOC_TYPE = "integer";

    String PCP_RCR = "pcprcr";
    String PCP_RCR_TYPE = "text_fr";
    //--------------------------------

}
