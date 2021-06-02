package fr.abes.periscope.core.entity.visualisation;

import lombok.Data;

@Data
public class Sequence {
    private BlocDebut blocDebut;
    private BlocFin blocFin;
    private String mentionDeLacune;
    private String texteEtatCollectionZone;
    private String note;
}
