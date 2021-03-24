package fr.abes.periscope.core.entity.v2.solr;

import fr.abes.periscope.core.entity.NoticeSolr;
import lombok.Getter;
import org.springframework.data.annotation.Id;
import org.springframework.data.solr.core.mapping.ChildDocument;
import org.springframework.data.solr.core.mapping.Indexed;

import java.io.Serializable;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

@Getter
public class NoticeV2Solr implements Serializable, NoticeSolr {

    @Id
    @Indexed(name = NoticeV2SolrField.PPN, type = NoticeV2SolrField.PPN_TYPE)
    private String ppn;

    @Indexed(name = NoticeV2SolrField.ISSN, type = NoticeV2SolrField.ISSN_TYPE)
    private String issn;

    @Indexed(name = NoticeV2SolrField.PCP_LIST, type = NoticeV2SolrField.PCP_LIST_TYPE)
    private HashSet<String> pcpList;

    @Indexed(name = NoticeV2SolrField.RCR_LIST, type = "string")
    private HashSet<String> rcrList;

    @Indexed(name = NoticeV2SolrField.EDITOR, type = "string")
    private String editor;

    @Indexed(name = NoticeV2SolrField.PROCESSING_GLOBAL_DATA, type = "string")
    private String processingGlobalData;

    @Indexed(name = NoticeV2SolrField.KEY_TITLE, type = "string")
    private String keyTitle;

    @Indexed(name = NoticeV2SolrField.KEY_SHORTED_TITLE, type = "string")
    private String keyShortedTitle;

    @Indexed(name = NoticeV2SolrField.PROPER_TITLE, type = "string")
    private String properTitle;

    @Indexed(name = NoticeV2SolrField.TITLE_FROM_DIFFERENT_AUTHOR, type = "string")
    private String titleFromDifferentAuthor;

    @Indexed(name = NoticeV2SolrField.PARALLEL_TITLE, type = "string")
    private String parallelTitle;

    @Indexed(name = NoticeV2SolrField.TITLE_COMPLEMENT, type = "string")
    private String titleComplement;

    @Indexed(name = NoticeV2SolrField.SECTION_TITLE, type = "string")
    private String sectionTitle;

    @Indexed(name = NoticeV2SolrField.KEY_TITLE_QUALIFIER, type = "string")
    private String keyTitleQualifer;

    @Indexed(name = NoticeV2SolrField.CONTINIOUS_TYPE, type = "string")
    private String continiousType;

    @Indexed(name = NoticeV2SolrField.EXTERNAL_URLS, type = "string")
    private List<String> externalURLs;

    @Indexed(name = NoticeV2SolrField.NB_LOC, type = "integer")
    private Integer nbLocation;

    @ChildDocument
    protected Set<ItemSolr> specimens = new HashSet<>();

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

        return ppn != null && ppn.equals(((NoticeV2Solr) obj).ppn);
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
