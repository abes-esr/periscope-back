package fr.abes.periscope.core.service;

import fr.abes.periscope.core.entity.visualisation.NoticeVisu;
import fr.abes.periscope.core.entity.xml.NoticesBibio;
import fr.abes.periscope.core.repository.baseXml.NoticesBibioRepository;
import fr.abes.periscope.core.util.BaseXmlMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class HoldingService {
    private BaseXmlMapper mapper;
    private NoticesBibioRepository repository;

    @Autowired
    public HoldingService(BaseXmlMapper mapper, NoticesBibioRepository repository) {
        this.mapper = mapper;
        this.repository = repository;
    }

    /**
     * Méthode permettant de récupérer une notice de la base xml et de la transformer au format notice + exemplaires + états de collection
     * @param ppn ppn de la notice à récupérer
     * @return notice + exemplaires + états de collection
     */
    public NoticeVisu getNoticeWithHoldings(String ppn) {
        NoticesBibio noticesBibio = repository.findFirstByPpn(ppn);
        return mapper.map(noticesBibio, NoticeVisu.class);
    }
}
