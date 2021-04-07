package fr.abes.periscope.core.entity.v2;

import fr.abes.periscope.core.entity.Notice;
import lombok.Getter;
import lombok.Setter;

import java.util.HashSet;
import java.util.Set;

@Getter @Setter
public class NoticeV2 extends Notice {

    protected Set<Item> items = new HashSet<>();

    public void addItem(Item item) {
        items.add(item);
    }
}
