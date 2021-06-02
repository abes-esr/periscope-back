package fr.abes.periscope.core.entity.visualisation;

import lombok.Data;
import org.apache.solr.client.solrj.beans.Field;

import javax.persistence.Id;
import java.util.LinkedList;
import java.util.List;

@Data
public class Lacune {
    private List<BlocDebut> blocs;
    private String commentaire;

    public Lacune() {
        blocs = new LinkedList<>();
    }

    public void addBloc(BlocDebut bloc) {
        this.blocs.add(bloc);
    }


}
