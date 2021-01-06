package fr.abes.periscope.core.entities;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.apache.solr.client.solrj.beans.Field;
import org.springframework.data.annotation.Id;
import org.springframework.data.solr.core.mapping.Indexed;
import org.springframework.data.solr.core.mapping.SolrDocument;

import java.util.Date;
import java.util.List;

@NoArgsConstructor
@Getter @Setter
@SolrDocument
public class Notice {

    @Id
    @Indexed(name = "001_s", type = "string")
    private String ppn;

    @Indexed(name = "011-a_z", type = "string")
    private String issn;

    //@Indexed(name = "011-a_t", type = "string")
    //private String keyTitle;

    @Indexed(name = "930-z_t", type = "string")
    private List<String> pcpList;

    @Indexed(name = "930-b_t", type = "string")
    private List<String> rcrList;

    @Indexed(name = "210-c_z", type = "string")
    private String editor;

    @Indexed(name = "100-a_t", type = "string")
    private String processingGlobalData;

    @Indexed(name = "530-a_z", type = "string")
    private String keyTitle;

    @Indexed(name = "531-a_z", type = "string")
    private String keyShortedTitle;

    @Indexed(name = "200-a_z", type = "string")
    private String properTitle;

    @Indexed(name = "200-c_z", type = "string")
    private String titleFromDifferentAuthor;

    @Indexed(name = "200-d_z", type = "string")
    private String parallelTitle;

    @Indexed(name = "200-e_z", type = "string")
    private String titleComplement;

    @Indexed(name = "200-i_z", type = "string")
    private String sectionTitle;

    @Indexed(name = "530-d_z", type = "string")
    private String keyTitleQualifer;

    @Indexed(name = "110-a_z", type = "string")
    private String continiousType;

    public Notice(String ppn) {
        this.ppn = ppn;
    }

    @Override
    public String toString() {
        return "Notice {"+ "ppn="+ ppn+", issn="+issn+"}";
    }
}
