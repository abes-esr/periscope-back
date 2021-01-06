package fr.abes.periscope.web.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Getter;
import lombok.Setter;
import org.springframework.data.solr.core.mapping.Indexed;

import java.util.Date;
import java.util.List;

@Getter @Setter
public class NoticeDto {

    @JsonProperty("ppn")
    private String ppn;

    private String issn;

    private List<String> pcpList;

    private List<String> rcrList;

    private String editor;

    private String processingGlobalData;

    private String keyTitle;

    private String keyShortedTitle;

    private String properTitle;

    private String titleFromDifferentAuthor;

    private String parallelTitle;

    private String titleComplement;

    private String sectionTitle;

    private String keyTitleQualifer;

    private String continiousType;

}
