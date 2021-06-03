package fr.abes.periscope.core.entity.visualisation;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class Sequence {
    private BlocDebut blocDebut;
    private BlocFin blocFin;
    private String mentionDeLacune;
    private String texteEtatCollectionZone;
    private String note;
    private EnumTypes type = EnumTypes.CONTINUE;
}
