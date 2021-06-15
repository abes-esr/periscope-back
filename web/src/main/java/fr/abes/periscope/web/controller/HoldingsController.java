package fr.abes.periscope.web.controller;

import fr.abes.periscope.core.service.HoldingService;
import fr.abes.periscope.web.dto.NoticeVisuWebDto;
import fr.abes.periscope.web.util.DtoMapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.*;

import java.io.IOException;
import java.sql.SQLException;

@Slf4j
@CrossOrigin(origins = "*")
@RestController
@RequestMapping("/api/v2")
public class HoldingsController {
    private final HoldingService service;
    private final DtoMapper mapper;

    public HoldingsController(HoldingService service, DtoMapper mapper) {
        this.service = service;
        this.mapper = mapper;
    }

    @GetMapping("/holdings/{ppn}")
    public NoticeVisuWebDto getHoldings(@PathVariable String ppn) throws SQLException, IOException {
        NoticeVisuWebDto notice = mapper.map(service.getNoticeWithHoldings(ppn), NoticeVisuWebDto.class);
        return notice;
    }
}
