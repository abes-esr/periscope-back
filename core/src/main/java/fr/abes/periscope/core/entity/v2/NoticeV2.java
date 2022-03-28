package fr.abes.periscope.core.entity.v2;

import fr.abes.periscope.core.entity.Item;
import fr.abes.periscope.core.entity.Notice;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.HashSet;
import java.util.Set;

@Getter @Setter
@NoArgsConstructor
public class NoticeV2 extends Notice {

    private String keyShortedTitle;

    private String properTitle;

    private String titleFromDifferentAuthor;

    private String parallelTitle;

    private String titleComplement;

    private String sectionTitle;

    private String keyTitleQualifer;

    private String language;

    private String country;

    private Integer nbPcp;

    private Set<String> pcpList = new HashSet<>();

    private Set<Item> items = new HashSet<>();

    public void addItem(Item item) {
        items.add(item);
    }

    public void addPcp(String pcp) { this.pcpList.add(pcp); }
}
