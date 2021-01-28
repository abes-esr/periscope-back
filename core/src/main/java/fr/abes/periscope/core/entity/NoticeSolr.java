package fr.abes.periscope.core.entity;

import fr.abes.periscope.core.repository.solr.NoticeField;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.springframework.data.annotation.Id;
import org.springframework.data.solr.core.mapping.Indexed;
import org.springframework.data.solr.core.mapping.SolrDocument;

import java.io.Serializable;
import java.util.HashSet;

/**
 * Représente une notice au format SolR
 */
@NoArgsConstructor
@Getter @Setter
@SolrDocument
public class NoticeSolr implements Serializable {

    @Id
    @Indexed(name = NoticeField.PPN, type = "string")
    private String ppn;

    @Indexed(name = NoticeField.ISSN, type = "string")
    private String issn;

    @Indexed(name = NoticeField.PCP_LIST, type = "string")
    private HashSet<String> pcpList;

    @Indexed(name = NoticeField.RCR_LIST, type = "string")
    private HashSet<String> rcrList;

    @Indexed(name = NoticeField.EDITOR, type = "string")
    private String editor;

    @Indexed(name = NoticeField.PROCESSING_GLOBAL_DATA, type = "string")
    private String processingGlobalData;

    @Indexed(name = NoticeField.KEY_TITLE, type = "string")
    private String keyTitle;

    @Indexed(name = NoticeField.KEY_SHORTED_TITLE, type = "string")
    private String keyShortedTitle;

    @Indexed(name = NoticeField.PROPER_TITLE, type = "string")
    private String properTitle;

    @Indexed(name = NoticeField.TITLE_FROM_DIFFERENT_AUTHOR, type = "string")
    private String titleFromDifferentAuthor;

    @Indexed(name = NoticeField.PARALLEL_TITLE, type = "string")
    private String parallelTitle;

    @Indexed(name = NoticeField.TITLE_COMPLEMENT, type = "string")
    private String titleComplement;

    @Indexed(name = NoticeField.SECTION_TITLE, type = "string")
    private String sectionTitle;

    @Indexed(name = NoticeField.KEY_TITLE_QUALIFIER, type = "string")
    private String keyTitleQualifer;

    @Indexed(name = NoticeField.CONTINIOUS_TYPE, type = "string")
    private String continiousType;

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

        return ppn != null && ppn.equals(((NoticeSolr) obj).ppn);
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
