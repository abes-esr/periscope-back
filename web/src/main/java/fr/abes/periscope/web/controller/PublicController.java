package fr.abes.periscope.web.controller;

import fr.abes.periscope.core.entities.Notice;
import fr.abes.periscope.core.service.NoticeStoreService;
import fr.abes.periscope.web.configuration.DtoMapperUtility;
import fr.abes.periscope.web.dto.NoticeDto;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * Controlleur d'API RESTful pour toutes les routes publiques.
 * Cette classe est basée sur le framework Spring avec le module Spring Web.
 * @since 0.0.1
 * @author Duy Tran
 */
@Slf4j
//@CrossOrigin(origins = "${application.crossorigin}")
@RestController
@RequestMapping("/api")
public class PublicController {

    private final NoticeStoreService noticeStoreService;

    /** Service pour le mapping DTO */
    @Autowired
    private DtoMapperUtility dtoMapper;

    @Autowired
    public PublicController(NoticeStoreService service) {
        noticeStoreService = service;
    }

    @GetMapping("/ppn")
    public NoticeDto plan(@RequestParam String ppn) throws Exception {

        Notice candidate = noticeStoreService.findByPpn(ppn);

        if (candidate != null) {
            return dtoMapper.map(candidate, NoticeDto.class);
        } else {
            throw new Exception("Pcp introuvable");
        }
    }

    @GetMapping("/pcp/dto/{page}/{size}")
    public List<NoticeDto> byPcp(@RequestParam String pcp, @PathVariable int page, @PathVariable int size) throws Exception {

        List<Notice> candidate = noticeStoreService.findNoticesByPcp(pcp,page,size);

        log.debug("List size is "+candidate.size());

        if (candidate != null) {
            return dtoMapper.mapList(candidate, NoticeDto.class);
        } else {
            throw new Exception("Pcp introuvable");
        }
    }

    @GetMapping("/pcp/complex/{page}/{size}")
    public List<NoticeDto> byPcpComplex(@RequestParam String pcp, @PathVariable int page, @PathVariable int size) throws Exception {

        List<Notice> candidate = noticeStoreService.findNoticesByPcpComplex(pcp,page,size);

        log.debug("List size is "+candidate.size());

        if (candidate != null) {
            return dtoMapper.mapList(candidate, NoticeDto.class);
        } else {
            throw new Exception("Pcp introuvable");
        }
    }

    @GetMapping("/pcp/entity")
    public List<Notice> byPcpEntity(@RequestParam String pcp, @PathVariable int page, @PathVariable int size) throws Exception {

        List<Notice> candidate = noticeStoreService.findNoticesByPcp(pcp,page,size);

        log.debug("List size is "+candidate.size());

        return candidate;
    }

}
