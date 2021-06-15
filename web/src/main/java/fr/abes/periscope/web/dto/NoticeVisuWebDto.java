package fr.abes.periscope.web.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Getter;
import lombok.Setter;

import java.util.ArrayList;
import java.util.List;

@Getter
@Setter
public class NoticeVisuWebDto  {
    @JsonProperty("holdings")
    private List<HoldingWebDto> holdingWebDtoList = new ArrayList<>();

    public void addHolding(HoldingWebDto holding) {
        this.holdingWebDtoList.add(holding);
    }

}
