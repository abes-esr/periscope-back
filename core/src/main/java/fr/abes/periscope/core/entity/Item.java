package fr.abes.periscope.core.entity;

import fr.abes.periscope.core.exception.IllegalValueException;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;

import java.util.List;

/**
 * Représente un exemplaire SUDOC
 */
@Slf4j
@Getter @Setter
@NoArgsConstructor
public class Item {

    protected String epn;

    protected String ppn;

    protected String rcr;

    protected List<String> pcp;

    public Item(String epn) {
        if (epn.matches("(\\d{1,9}X?)(\\d{2})?")) {
            this.epn = epn;
        } else {
            throw new IllegalValueException("EPN do not respect the format : 9 to 11 numbers");
        }
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

        return epn != null && epn.equals(((Item) obj).epn);
    }

    @Override
    public int hashCode() {
        return 2021;
    }

    @Override
    public String toString() {
        return "Item {"+ "epn="+ epn+" " + ((pcp!=null) ? " pcp="+pcp+" " : "") +"}";
    }
}
