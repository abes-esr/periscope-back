package fr.abes.periscope.web.controller;

import fr.abes.periscope.core.entity.Notice;
import fr.abes.periscope.core.service.NoticeStoreService;
import fr.abes.periscope.web.util.DtoMapper;
import fr.abes.periscope.web.dto.NoticeWebDto;
import fr.abes.periscope.core.util.TrackExecutionTime;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@Slf4j
@CrossOrigin(origins = "${application.crossorigin}")
@RestController
@RequestMapping("/api")
public class PublicController {

    private final NoticeStoreService noticeStoreService;

    /** Service pour le mapping DTO */
    @Autowired
    private DtoMapper dtoMapper;

    @Autowired
    public PublicController(NoticeStoreService service) {
        noticeStoreService = service;
    }

    @TrackExecutionTime
    @GetMapping("/ppn")
    public NoticeWebDto plan(@RequestParam String ppn) throws Exception {

        Notice candidate = noticeStoreService.findByPpn(ppn);

        if (candidate != null) {
            return dtoMapper.map(candidate, NoticeWebDto.class);
        } else {
            throw new Exception("Pcp introuvable");
        }
    }

    @TrackExecutionTime
    @GetMapping("/pcp/dto/{page}/{size}")
    public List<NoticeWebDto> byPcp(@RequestParam String pcp, @PathVariable int page, @PathVariable int size) throws Exception {

        List<Notice> candidate = noticeStoreService.findNoticesByPcp(pcp,page,size);

        log.debug("List size is "+candidate.size());

        return dtoMapper.mapList(candidate, NoticeWebDto.class);

    }

    @TrackExecutionTime
    @GetMapping("/pcp/complex/{page}/{size}")
    public List<NoticeWebDto> byPcpComplex(@RequestParam String pcp, @PathVariable int page, @PathVariable int size) throws Exception {

        List<Notice> candidate = noticeStoreService.findNoticesByPcpComplex(pcp,page,size);

        log.debug("List size is "+candidate.size());

        return dtoMapper.mapList(candidate, NoticeWebDto.class);
    }

    @TrackExecutionTime
    @GetMapping("/pcp/entity")
    public List<Notice> byPcpEntity(@RequestParam String pcp, @PathVariable int page, @PathVariable int size) throws Exception {

        List<Notice> candidate = noticeStoreService.findNoticesByPcp(pcp,page,size);

        log.debug("List size is "+candidate.size());

        return candidate;
    }

}
