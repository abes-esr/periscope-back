package fr.abes.periscope.core.entity.v2.solr;

import lombok.Getter;
import lombok.Setter;
import org.apache.solr.client.solrj.beans.Field;
import org.springframework.data.annotation.Id;
import org.springframework.data.solr.core.mapping.Indexed;
import org.springframework.data.solr.core.mapping.SolrDocument;

import java.io.Serializable;
import java.util.List;

/**
 * Représente un exemplaire
 */
@Getter @Setter
@SolrDocument
public class ItemSolr implements Serializable {

    @Id
    @Field(ItemSolrField.ID)
    @Indexed(name = ItemSolrField.ID)
    private String id;

    @Field(ItemSolrField.EPN)
    @Indexed(name = ItemSolrField.EPN)
    private String epn;

    @Field(ItemSolrField.PPN_PARENT)
    @Indexed(name = ItemSolrField.PPN_PARENT)
    private String ppn;

    @Field(NoticeV2SolrField.TITLE_TYPE)
    @Indexed(name = NoticeV2SolrField.TITLE_TYPE)
    private String titleType = "exemplaire";

    @Field(ItemSolrField.RCR)
    @Indexed(name = ItemSolrField.RCR)
    private String rcr;

    @Field(ItemSolrField.PCP)
    @Indexed(name = ItemSolrField.PCP)
    private List<String> pcp;

    public ItemSolr(String ppn, String epn) {
        this.id = epn;
        this.epn = epn;
        this.ppn = ppn; // Lien avec la notice parent
    }

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

        return this.id != null && id.equals(((ItemSolr) obj).id);
    }

    @Override
    public int hashCode() {
        return 2020;
    }

    @Override
    public String toString() {
        return "ItemSolr {"+ "id="+ id+", epn="+epn+", rcr="+rcr+"}";
    }
}
