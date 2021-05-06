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
    private String editor;

    @Field(NoticeV2SolrField.PROCESSING_GLOBAL_DATA)
    @Indexed(name = NoticeV2SolrField.PROCESSING_GLOBAL_DATA)
    private String processingGlobalData;

    @Field(NoticeV2SolrField.KEY_TITLE)
    @Indexed(name = NoticeV2SolrField.KEY_TITLE)
    private String keyTitle;

    @Field(NoticeV2SolrField.KEY_SHORTED_TITLE)
    @Indexed(name = NoticeV2SolrField.KEY_SHORTED_TITLE)
    private String keyShortedTitle;

    @Field(NoticeV2SolrField.PROPER_TITLE)
    @Indexed(name = NoticeV2SolrField.PROPER_TITLE)
    private String properTitle;

    @Field(NoticeV2SolrField.TITLE_FROM_DIFFERENT_AUTHOR)
    @Indexed(name = NoticeV2SolrField.TITLE_FROM_DIFFERENT_AUTHOR)
    private String titleFromDifferentAuthor;

    @Field(NoticeV2SolrField.PARALLEL_TITLE)
    @Indexed(name = NoticeV2SolrField.PARALLEL_TITLE)
    private String parallelTitle;

    @Field(NoticeV2SolrField.TITLE_COMPLEMENT)
    @Indexed(name = NoticeV2SolrField.TITLE_COMPLEMENT)
    private String titleComplement;

    @Field(NoticeV2SolrField.SECTION_TITLE)
    @Indexed(name = NoticeV2SolrField.SECTION_TITLE)
    private String sectionTitle;

    @Field(NoticeV2SolrField.KEY_TITLE_QUALIFIER)
    @Indexed(name = NoticeV2SolrField.KEY_TITLE_QUALIFIER)
    private String keyTitleQualifer;

    @Field(NoticeV2SolrField.DOCUMENT_TYPE)
    @Indexed(name = NoticeV2SolrField.DOCUMENT_TYPE)
    private String typeDocument;

    @Field(NoticeV2SolrField.EXTERNAL_URLS)
    @Indexed(name = NoticeV2SolrField.EXTERNAL_URLS)
    private List<String> externalURLs;

    @Field(NoticeV2SolrField.NB_LOC)
    @Indexed(name = NoticeV2SolrField.NB_LOC)
    private Integer nbLocation;

    @Field(NoticeV2SolrField.LANGUAGE)
    @Indexed(name = NoticeV2SolrField.LANGUAGE)
    protected String language;

    @Field(NoticeV2SolrField.COUNTRY)
    @Indexed(name = NoticeV2SolrField.COUNTRY)
    protected String country;

    @Field(NoticeV2SolrField.START_YEAR)
    @Indexed(name = NoticeV2SolrField.START_YEAR)
    protected Integer startYear;

    @Field(NoticeV2SolrField.START_YEAR_CONFIDENCE_INDEX)
    @Indexed(name = NoticeV2SolrField.START_YEAR_CONFIDENCE_INDEX)
    protected Integer startYearConfidenceIndex;

    @Field(NoticeV2SolrField.END_YEAR)
    @Indexed(name = NoticeV2SolrField.END_YEAR)
    protected Integer endYear;

    @Field(NoticeV2SolrField.END_YEAR_CONFIDENCE_INDEX)
    @Indexed(name = NoticeV2SolrField.END_YEAR_CONFIDENCE_INDEX)
    protected Integer endYearConfidenceIndex;

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
