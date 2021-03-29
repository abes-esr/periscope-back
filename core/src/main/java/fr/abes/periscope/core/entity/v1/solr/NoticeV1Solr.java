package fr.abes.periscope.core.entity.v1.solr;

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
 * @Deprecated Représente une notice au format SolR
 */
@Deprecated
@NoArgsConstructor
@Getter @Setter
@SolrDocument
public class NoticeV1Solr implements Serializable {

    @Id
    @Indexed(name = NoticeV1SolrField.PPN, type = "string")
    private String ppn;

    @Indexed(name = NoticeV1SolrField.ISSN, type = "string")
    private String issn;

    @Indexed(name = NoticeV1SolrField.PCP_LIST, type = "string")
    private HashSet<String> pcpList;

    @Indexed(name = NoticeV1SolrField.RCR_LIST, type = "string")
    private HashSet<String> rcrList;

    @Indexed(name = NoticeV1SolrField.EDITOR, type = "string")
    private String editor;

    @Indexed(name = NoticeV1SolrField.PROCESSING_GLOBAL_DATA, type = "string")
    private String processingGlobalData;

    @Indexed(name = NoticeV1SolrField.KEY_TITLE, type = "string")
    private String keyTitle;

    @Indexed(name = NoticeV1SolrField.KEY_SHORTED_TITLE, type = "string")
    private String keyShortedTitle;

    @Indexed(name = NoticeV1SolrField.PROPER_TITLE, type = "string")
    private String properTitle;

    @Indexed(name = NoticeV1SolrField.TITLE_FROM_DIFFERENT_AUTHOR, type = "string")
    private String titleFromDifferentAuthor;

    @Indexed(name = NoticeV1SolrField.PARALLEL_TITLE, type = "string")
    private String parallelTitle;

    @Indexed(name = NoticeV1SolrField.TITLE_COMPLEMENT, type = "string")
    private String titleComplement;

    @Indexed(name = NoticeV1SolrField.SECTION_TITLE, type = "string")
    private String sectionTitle;

    @Indexed(name = NoticeV1SolrField.KEY_TITLE_QUALIFIER, type = "string")
    private String keyTitleQualifer;

    @Indexed(name = NoticeV1SolrField.CONTINIOUS_TYPE, type = "string")
    private String continiousType;

    @Indexed(name = NoticeV1SolrField.EXTERNAL_URLS_S, type = "string")
    private List<String> externalURLs;

    @Indexed(name = NoticeV1SolrField.NB_LOC, type = "integer")
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

        return ppn != null && ppn.equals(((NoticeV1Solr) obj).ppn);
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
