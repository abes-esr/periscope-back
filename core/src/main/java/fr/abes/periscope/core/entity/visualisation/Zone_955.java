package fr.abes.periscope.core.entity.visualisation;

import lombok.Data;

@Data
public class Zone_955 {

    private String id;
    private String epn;

    private Sequence sequencePrimaire;
    private Sequence sequenceParallele;

    private String zone_955_r;
    private String zone_955_w;
    private String zone_955_z;

    public Zone_955(String epn) {
        this.epn = epn;
    }

}
