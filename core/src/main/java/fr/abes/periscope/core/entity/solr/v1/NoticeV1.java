package fr.abes.periscope.core.entity.solr.v1;

import fr.abes.periscope.core.entity.solr.Notice;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.HashSet;
import java.util.Set;

/**
 * Représente une Notice SUDOC dans sa version 1 selon le SolR4a
 */
@Getter
@Setter
@Deprecated
@NoArgsConstructor
public class NoticeV1 extends Notice {
    private String keyShortedTitle;

    private String properTitle;

    private String titleFromDifferentAuthor;

    private String parallelTitle;

    private String titleComplement;

    private String sectionTitle;

    private String keyTitleQualifer;

    private Set<String> rcrList = new HashSet<>();

}
