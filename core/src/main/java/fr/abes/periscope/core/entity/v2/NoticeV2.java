package fr.abes.periscope.core.entity.v2;

import fr.abes.periscope.core.entity.Notice;

import java.util.HashSet;
import java.util.Set;

public class NoticeV2 extends Notice {

    protected Set<Item> specimens = new HashSet<>();
}
