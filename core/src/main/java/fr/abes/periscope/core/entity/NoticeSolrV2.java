package fr.abes.periscope.core.entity;

import org.springframework.data.annotation.Id;
import org.springframework.data.solr.core.mapping.Indexed;

import java.io.Serializable;
import java.util.HashSet;
import java.util.List;

public class NoticeSolrV2 extends NoticeSolr implements Serializable {

    @Id
    @Indexed(name = NoticeV2Field.PPN, type = NoticeV2Field.PPN_TYPE)
    private String ppn;

    @Indexed(name = NoticeV2Field.ISSN, type = NoticeV2Field.ISSN_TYPE)
    private String issn;

    @Indexed(name = NoticeV2Field.PCP_LIST, type = NoticeV2Field.PCP_LIST_TYPE)
    private HashSet<String> pcpList;

    @Indexed(name = NoticeV2Field.RCR_LIST, type = "string")
    private HashSet<String> rcrList;

    @Indexed(name = NoticeV2Field.EDITOR, type = "string")
    private String editor;

    @Indexed(name = NoticeV2Field.PROCESSING_GLOBAL_DATA, type = "string")
    private String processingGlobalData;

    @Indexed(name = NoticeV2Field.KEY_TITLE, type = "string")
    private String keyTitle;

    @Indexed(name = NoticeV2Field.KEY_SHORTED_TITLE, type = "string")
    private String keyShortedTitle;

    @Indexed(name = NoticeV2Field.PROPER_TITLE, type = "string")
    private String properTitle;

    @Indexed(name = NoticeV2Field.TITLE_FROM_DIFFERENT_AUTHOR, type = "string")
    private String titleFromDifferentAuthor;

    @Indexed(name = NoticeV2Field.PARALLEL_TITLE, type = "string")
    private String parallelTitle;

    @Indexed(name = NoticeV2Field.TITLE_COMPLEMENT, type = "string")
    private String titleComplement;

    @Indexed(name = NoticeV2Field.SECTION_TITLE, type = "string")
    private String sectionTitle;

    @Indexed(name = NoticeV2Field.KEY_TITLE_QUALIFIER, type = "string")
    private String keyTitleQualifer;

    @Indexed(name = NoticeV2Field.CONTINIOUS_TYPE, type = "string")
    private String continiousType;

    @Indexed(name = NoticeV2Field.NB_LOC, type = "integer")
    private Integer nbLocation;

    @Override
    public boolean equals(Object obj) {
        if (obj == null) {
            return false;
        }

        if (this == obj) {
            return true;
        }

        if (getClass() != obj.getClass()) {
            return false;
        }

        return ppn != null && ppn.equals(((NoticeSolrV2) obj).ppn);
    }

    @Override
    public int hashCode() {
        return 2020;
    }

    @Override
    public String toString() {
        return "NoticeSolr {"+ "ppn="+ ppn+", issn="+issn+"}";
    }
}
