package fr.abes.periscope.core.entity.v1;

import fr.abes.periscope.core.entity.Notice;
import lombok.Getter;
import lombok.Setter;

import java.util.HashSet;
import java.util.Set;

/**
 * Représente une Notice SUDOC dans sa version 1 selon le SolR4a
 */
@Getter
@Setter
@Deprecated
public class NoticeV1 extends Notice {

    private Set<String> rcrList = new HashSet<>();

}
