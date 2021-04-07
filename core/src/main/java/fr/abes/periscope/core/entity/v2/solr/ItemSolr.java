package fr.abes.periscope.core.entity.v2.solr;

import lombok.Getter;
import lombok.Setter;
import org.apache.solr.client.solrj.beans.Field;
import org.springframework.data.annotation.Id;
import org.springframework.data.solr.core.mapping.Indexed;
import org.springframework.data.solr.core.mapping.SolrDocument;

import java.io.Serializable;

/**
 * Représente un exemplaire
 */
@Getter @Setter
@SolrDocument
public class ItemSolr implements Serializable {

    @Id
    @Field(ItemSolrField.ID)
    @Indexed(name = ItemSolrField.ID, type = ItemSolrField.ID_TYPE)
    private String id;

    @Field(ItemSolrField.EPN)
    @Indexed(name = ItemSolrField.EPN, type = ItemSolrField.EPN_TYPE)
    private String epn;

    @Field(NoticeV2SolrField.PPN)
    @Indexed(name = NoticeV2SolrField.PPN, type = NoticeV2SolrField.PPN_TYPE)
    private String ppn;

    @Field(NoticeV2SolrField.TITLE_TYPE)
    @Indexed(name = NoticeV2SolrField.TITLE_TYPE, type = NoticeV2SolrField.TITLE_TYPE_TYPE)
    private String type = "exemplaire";

    @Field(ItemSolrField.RCR)
    @Indexed(name = ItemSolrField.RCR, type = ItemSolrField.RCR_TYPE)
    private String rcr;

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
