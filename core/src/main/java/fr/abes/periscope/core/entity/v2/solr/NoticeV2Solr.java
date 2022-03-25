package fr.abes.periscope.core.entity.v2.solr;

import lombok.Getter;
import lombok.Setter;
import org.apache.solr.client.solrj.beans.Field;
import org.springframework.data.annotation.Id;
import org.springframework.data.solr.core.mapping.ChildDocument;
import org.springframework.data.solr.core.mapping.Indexed;
import org.springframework.data.solr.core.mapping.SolrDocument;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Représente une notice au format SolR selon le SolR V2 de PERISCOPE
 */
@Getter @Setter
@SolrDocument
public class NoticeV2Solr implements Serializable {

    @Id
    @Field(NoticeV2SolrField.ID)
    @Indexed(name = NoticeV2SolrField.ID)
    private String id;

    @Field(NoticeV2SolrField.TITLE_TYPE)
    @Indexed(name = NoticeV2SolrField.TITLE_TYPE)
    private String titleType = "notice";

    @Field(NoticeV2SolrField.PPN)
    @Indexed(name = NoticeV2SolrField.PPN)
    private String ppn;

    @Field(NoticeV2SolrField.ISSN)
    @Indexed(name = NoticeV2SolrField.ISSN)
    private String issn;

    @Field(NoticeV2SolrField.EDITOR)
    @Indexed(name = NoticeV2SolrField.EDITOR)
    protected List<String> editor = new ArrayList<>();

    @Field(NoticeV2SolrField.EDITOR_Z)
    @Indexed(name = NoticeV2SolrField.EDITOR_Z)
    protected String editorForDisplay;

    @Field(NoticeV2SolrField.PROCESSING_GLOBAL_DATA)
    @Indexed(name = NoticeV2SolrField.PROCESSING_GLOBAL_DATA)
    protected String processingGlobalData;

    @Field(NoticeV2SolrField.PROPER_TITLE_Z)
    @Indexed(name = NoticeV2SolrField.PROPER_TITLE_Z)
    protected String properTitleForDisplay;

    @Field(NoticeV2SolrField.PROPER_TITLE)
    @Indexed(name = NoticeV2SolrField.PROPER_TITLE)
    protected List<String> properTitle = new ArrayList<>();

    @Field(NoticeV2SolrField.TITLE_FROM_DIFFERENT_AUTHOR_Z)
    @Indexed(NoticeV2SolrField.TITLE_FROM_DIFFERENT_AUTHOR_Z)
    protected String titleFromDifferentAuthorForDisplay;

    @Field(NoticeV2SolrField.TITLE_FROM_DIFFERENT_AUTHOR)
    @Indexed(name = NoticeV2SolrField.TITLE_FROM_DIFFERENT_AUTHOR)
    protected List<String> titleFromDifferentAuthor = new ArrayList<>();

    @Field(NoticeV2SolrField.PARALLEL_TITLE_Z)
    @Indexed(name = NoticeV2SolrField.PARALLEL_TITLE_Z)
    protected String parallelTitleForDisplay;

    @Field(NoticeV2SolrField.PARALLEL_TITLE)
    @Indexed(name = NoticeV2SolrField.PARALLEL_TITLE)
    protected List<String> parallelTitle = new ArrayList<>();

    @Field(NoticeV2SolrField.TITLE_COMPLEMENT_Z)
    @Indexed(name = NoticeV2SolrField.TITLE_COMPLEMENT_Z)
    protected String titleComplementForDisplay;

    @Field(NoticeV2SolrField.TITLE_COMPLEMENT)
    @Indexed(name = NoticeV2SolrField.TITLE_COMPLEMENT)
    protected List<String> titleComplement = new ArrayList<>();

    @Field(NoticeV2SolrField.SECTION_TITLE_Z)
    @Indexed(name = NoticeV2SolrField.SECTION_TITLE_Z)
    protected String sectionTitleForDisplay;

    @Field(NoticeV2SolrField.SECTION_TITLE)
    @Indexed(name = NoticeV2SolrField.SECTION_TITLE)
    protected List<String> sectionTitle = new ArrayList<>();

    @Field(NoticeV2SolrField.KEY_TITLE)
    @Indexed(name = NoticeV2SolrField.KEY_TITLE)
    protected String keyTitle;

    @Field(NoticeV2SolrField.KEY_TITLE_QUALIFIER)
    @Indexed(name = NoticeV2SolrField.KEY_TITLE_QUALIFIER)
    protected String keyTitleQualifer;

    @Field(NoticeV2SolrField.KEY_SHORTED_TITLE_Z)
    @Indexed(name = NoticeV2SolrField.KEY_SHORTED_TITLE_Z)
    protected String keyShortedTitleForDisplay;

    @Field(NoticeV2SolrField.KEY_SHORTED_TITLE)
    @Indexed(name = NoticeV2SolrField.KEY_SHORTED_TITLE)
    protected List<String> keyShortedTitle = new ArrayList<>();

    @Field(NoticeV2SolrField.DOCUMENT_TYPE)
    @Indexed(name = NoticeV2SolrField.DOCUMENT_TYPE)
    protected String continuousType;

    @Field(NoticeV2SolrField.SUPPORT_TYPE)
    @Indexed(name = NoticeV2SolrField.SUPPORT_TYPE)
    protected String supportType;

    @Field(NoticeV2SolrField.EXTERNAL_URLS)
    @Indexed(name = NoticeV2SolrField.EXTERNAL_URLS)
    protected List<String> externalURLs = new ArrayList<>();

    @Field(NoticeV2SolrField.NB_LOC)
    @Indexed(name = NoticeV2SolrField.NB_LOC)
    protected Integer nbLocation;

    @Field(NoticeV2SolrField.NB_PCP)
    @Indexed(name = NoticeV2SolrField.NB_PCP)
    protected Integer nbPcp;

    @Field(NoticeV2SolrField.LANGUAGE)
    @Indexed(name = NoticeV2SolrField.LANGUAGE)
    protected String language;

    @Field(NoticeV2SolrField.COUNTRY)
    @Indexed(name = NoticeV2SolrField.COUNTRY)
    protected String country;

    @Field(NoticeV2SolrField.START_YEAR)
    @Indexed(name = NoticeV2SolrField.START_YEAR)
    protected String startYear;

    @Field(NoticeV2SolrField.START_YEAR_CONFIDENCE_INDEX)
    @Indexed(name = NoticeV2SolrField.START_YEAR_CONFIDENCE_INDEX)
    protected Integer startYearConfidenceIndex;

    @Field(NoticeV2SolrField.END_YEAR)
    @Indexed(name = NoticeV2SolrField.END_YEAR)
    protected String endYear;

    @Field(NoticeV2SolrField.END_YEAR_CONFIDENCE_INDEX)
    @Indexed(name = NoticeV2SolrField.END_YEAR_CONFIDENCE_INDEX)
    protected Integer endYearConfidenceIndex;

    @Field(NoticeV2SolrField.RCR_LIST)
    @Indexed(name = NoticeV2SolrField.RCR_LIST)
    protected Set<String> rcrList = new HashSet<>();

    @Field(NoticeV2SolrField.PCP_LIST)
    @Indexed(name = NoticeV2SolrField.PCP_LIST)
    protected Set<String> pcpList = new HashSet<>();

    @ChildDocument
    private Set<ItemSolr> items = new HashSet<>();

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
