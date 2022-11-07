package fr.abes.periscope.web.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Getter;
import lombok.Setter;

import java.util.ArrayList;
import java.util.List;

@Getter
@Setter
public class ResultWebDto {
    @JsonProperty("notice")
    private List<NoticeWebV2Dto> notices;
    @JsonProperty("facettes")
    private List<FacetteWebDto> facettes = new ArrayList<>();
    @JsonProperty("nbPages")
    private int nbPages;
    @JsonProperty("nbNotices")
    private long nbNotices;

    public void addFacette(FacetteWebDto facette) {
        this.facettes.add(facette);
    }
}
