package fr.abes.periscope.core.entity;

import lombok.Getter;
import lombok.Setter;

/**
 * Représente un exemplaire SUDOC
 */
@Getter @Setter
public class Item {

    protected String epn;

    protected String ppn;

    protected String rcr;

    protected String pcp;

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

        return epn != null && epn.equals(((Item) obj).epn);
    }

    @Override
    public int hashCode() {
        return 2021;
    }

    @Override
    public String toString() {
        return "Item {"+ "epn="+ epn+" }";
    }
}
