package fr.abes.periscope.core.entity.v2.solr;

import lombok.Getter;
import lombok.Setter;
import org.apache.solr.client.solrj.beans.Field;
import org.springframework.data.annotation.Id;
import org.springframework.data.solr.core.mapping.ChildDocument;
import org.springframework.data.solr.core.mapping.Indexed;
import org.springframework.data.solr.core.mapping.SolrDocument;

import java.io.Serializable;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

@Getter @Setter
@SolrDocument
public class NoticeV2Solr implements Serializable {

    @Id
    @Field(NoticeV2SolrField.ID)
    @Indexed(name = NoticeV2SolrField.ID, type = NoticeV2SolrField.ID_TYPE)
    protected String id;

    @Field(NoticeV2SolrField.TITLE_TYPE)
    @Indexed(name = NoticeV2SolrField.TITLE_TYPE, type = NoticeV2SolrField.TITLE_TYPE_TYPE)
    private String type = "notice";

    @Field(NoticeV2SolrField.PPN)
    @Indexed(name = NoticeV2SolrField.PPN, type = NoticeV2SolrField.PPN_TYPE)
    protected String ppn;

    @Field(NoticeV2SolrField.ISSN)
    @Indexed(name = NoticeV2SolrField.ISSN, type = NoticeV2SolrField.ISSN_TYPE)
    protected String issn;

    @Field(NoticeV2SolrField.PCP_LIST)
    @Indexed(name = NoticeV2SolrField.PCP_LIST, type = NoticeV2SolrField.PCP_LIST_TYPE)
    @Deprecated
    protected Set<String> pcpList;

    @Field(NoticeV2SolrField.RCR_LIST)
    @Indexed(name = NoticeV2SolrField.RCR_LIST, type = NoticeV2SolrField.RCR_LIST_TYPE)
    @Deprecated
    protected Set<String> rcrList;

    @Field(NoticeV2SolrField.EDITOR)
    @Indexed(name = NoticeV2SolrField.EDITOR, type = NoticeV2SolrField.EDITOR_TYPE)
    protected String editor;

    @Field(NoticeV2SolrField.PROCESSING_GLOBAL_DATA)
    @Indexed(name = NoticeV2SolrField.PROCESSING_GLOBAL_DATA, type = NoticeV2SolrField.PROCESSING_GLOBAL_DATA_TYPE)
    protected String processingGlobalData;

    @Field(NoticeV2SolrField.KEY_TITLE)
    @Indexed(name = NoticeV2SolrField.KEY_TITLE, type = NoticeV2SolrField.KEY_TITLE_TYPE)
    protected String keyTitle;

    @Field(NoticeV2SolrField.KEY_SHORTED_TITLE)
    @Indexed(name = NoticeV2SolrField.KEY_SHORTED_TITLE, type = NoticeV2SolrField.KEY_SHORTED_TITLE_TYPE)
    protected String keyShortedTitle;

    @Field(NoticeV2SolrField.PROPER_TITLE)
    @Indexed(name = NoticeV2SolrField.PROPER_TITLE, type = NoticeV2SolrField.PROPER_TITLE_TYPE)
    protected String properTitle;

    @Field(NoticeV2SolrField.TITLE_FROM_DIFFERENT_AUTHOR)
    @Indexed(name = NoticeV2SolrField.TITLE_FROM_DIFFERENT_AUTHOR, type = NoticeV2SolrField.TITLE_FROM_DIFFERENT_AUTHOR_TYPE)
    protected String titleFromDifferentAuthor;

    @Field(NoticeV2SolrField.PARALLEL_TITLE)
    @Indexed(name = NoticeV2SolrField.PARALLEL_TITLE, type = NoticeV2SolrField.PARALLEL_TITLE_TYPE)
    protected String parallelTitle;

    @Field(NoticeV2SolrField.TITLE_COMPLEMENT)
    @Indexed(name = NoticeV2SolrField.TITLE_COMPLEMENT, type = NoticeV2SolrField.TITLE_COMPLEMENT_TYPE)
    protected String titleComplement;

    @Field(NoticeV2SolrField.SECTION_TITLE)
    @Indexed(name = NoticeV2SolrField.SECTION_TITLE, type = NoticeV2SolrField.SECTION_TITLE_TYPE)
    protected String sectionTitle;

    @Field(NoticeV2SolrField.KEY_TITLE_QUALIFIER)
    @Indexed(name = NoticeV2SolrField.KEY_TITLE_QUALIFIER, type = NoticeV2SolrField.KEY_TITLE_QUALIFIER_TYPE)
    protected String keyTitleQualifer;

    @Field(NoticeV2SolrField.CONTINIOUS_TYPE)
    @Indexed(name = NoticeV2SolrField.CONTINIOUS_TYPE, type = NoticeV2SolrField.CONTINIOUS_TYPE_TYPE)
    protected String continiousType;

    @Field(NoticeV2SolrField.EXTERNAL_URLS)
    @Indexed(name = NoticeV2SolrField.EXTERNAL_URLS, type = NoticeV2SolrField.EXTERNAL_URLS_TYPE)
    protected List<String> externalURLs;

    @Field(NoticeV2SolrField.NB_LOC)
    @Indexed(name = NoticeV2SolrField.NB_LOC, type = NoticeV2SolrField.NB_LOC_TYPE)
    protected Integer nbLocation;

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
