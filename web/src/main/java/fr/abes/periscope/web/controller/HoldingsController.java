package fr.abes.periscope.web.controller;

import fr.abes.periscope.core.exception.IllegalPpnException;
import fr.abes.periscope.core.service.HoldingService;
import fr.abes.periscope.core.util.UtilsMapper;
import fr.abes.periscope.web.dto.NoticeVisuWebDto;
import fr.abes.periscope.web.dto.SequenceWebDto;
import fr.abes.periscope.web.util.DtoMapper;
import fr.abes.periscope.web.util.TYPE_SEQUENCE;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.*;

import java.io.IOException;
import java.sql.SQLException;
import java.util.List;
import java.util.stream.Collectors;

@Slf4j
@CrossOrigin(origins = "*")
@RestController
@RequestMapping("/api/v2")
public class HoldingsController {
    private final HoldingService service;
    private final UtilsMapper mapper;

    public HoldingsController(HoldingService service, UtilsMapper mapper) {
        this.service = service;
        this.mapper = mapper;
    }

    /**
     * Web Service de récupération des états de collection
     * @param ppn de la notice à récupérer
     * @param typeSequence optionnel : précise le type de séquence à récupérer : LACUNE, ERREUR, CONTINUE
     * @return
     * @throws SQLException
     * @throws IOException
     */
    @GetMapping("/holdings/{ppn}")
    public NoticeVisuWebDto getHoldings(@PathVariable String ppn, @RequestParam(required = false) TYPE_SEQUENCE typeSequence) throws SQLException, IOException, IllegalPpnException {
        NoticeVisuWebDto notice = mapper.map(service.getNoticeWithHoldings(ppn), NoticeVisuWebDto.class);
        if (typeSequence != null) {
            notice.getHoldingWebDtoList().forEach(h -> {
                switch (typeSequence) {
                    case CONTINUE:
                        List<SequenceWebDto> continues = h.getSequencesList().stream().filter(s -> s.getTypeSequence().equals(TYPE_SEQUENCE.CONTINUE)).collect(Collectors.toList());
                        h.clearSequence();
                        h.addSequences(continues);
                        break;
                    case LACUNE:
                        List<SequenceWebDto> lacunes = h.getSequencesList().stream().filter(s -> s.getTypeSequence().equals(TYPE_SEQUENCE.LACUNE)).collect(Collectors.toList());
                        h.clearSequence();
                        h.addSequences(lacunes);
                        break;
                    default:
                        List<SequenceWebDto> erreurs = h.getSequencesList().stream().filter(s -> s.getTypeSequence().equals(TYPE_SEQUENCE.ERREUR)).collect(Collectors.toList());
                        h.clearSequence();
                        h.addSequences(erreurs);
                }
            });
        }
        notice.getHoldingWebDtoList().removeAll(notice.getHoldingWebDtoList().stream().filter(h -> h.getSequencesList().size()==0).collect(Collectors.toList()));
        return notice;
    }
}
