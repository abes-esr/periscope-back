package fr.abes.periscope.core.entity;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.springframework.data.annotation.Id;
import org.springframework.data.solr.core.mapping.Indexed;
import org.springframework.data.solr.core.mapping.SolrDocument;

import java.io.Serializable;
import java.util.HashSet;
import java.util.List;

/**
 * Représente une notice au format SolR
 */
@NoArgsConstructor
@Getter @Setter
@SolrDocument
public class NoticeSolrV1 extends NoticeSolr implements Serializable {

    @Id
    @Indexed(name = NoticeV1Field.PPN, type = "string")
    private String ppn;

    @Indexed(name = NoticeV1Field.ISSN, type = "string")
    private String issn;

    @Indexed(name = NoticeV1Field.PCP_LIST, type = "string")
    private HashSet<String> pcpList;

    @Indexed(name = NoticeV1Field.RCR_LIST, type = "string")
    private HashSet<String> rcrList;

    @Indexed(name = NoticeV1Field.EDITOR, type = "string")
    private String editor;

    @Indexed(name = NoticeV1Field.PROCESSING_GLOBAL_DATA, type = "string")
    private String processingGlobalData;

    @Indexed(name = NoticeV1Field.KEY_TITLE, type = "string")
    private String keyTitle;

    @Indexed(name = NoticeV1Field.KEY_SHORTED_TITLE, type = "string")
    private String keyShortedTitle;

    @Indexed(name = NoticeV1Field.PROPER_TITLE, type = "string")
    private String properTitle;

    @Indexed(name = NoticeV1Field.TITLE_FROM_DIFFERENT_AUTHOR, type = "string")
    private String titleFromDifferentAuthor;

    @Indexed(name = NoticeV1Field.PARALLEL_TITLE, type = "string")
    private String parallelTitle;

    @Indexed(name = NoticeV1Field.TITLE_COMPLEMENT, type = "string")
    private String titleComplement;

    @Indexed(name = NoticeV1Field.SECTION_TITLE, type = "string")
    private String sectionTitle;

    @Indexed(name = NoticeV1Field.KEY_TITLE_QUALIFIER, type = "string")
    private String keyTitleQualifer;

    @Indexed(name = NoticeV1Field.CONTINIOUS_TYPE, type = "string")
    private String continiousType;

    @Indexed(name = NoticeV1Field.EXTERNAL_URLS_S, type = "string")
    private List<String> externalURLs;

    @Indexed(name = NoticeV1Field.NB_LOC, type = "integer")
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

        return ppn != null && ppn.equals(((NoticeSolrV1) obj).ppn);
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
