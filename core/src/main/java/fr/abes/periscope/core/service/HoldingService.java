package fr.abes.periscope.core.service;

import com.fasterxml.jackson.dataformat.xml.JacksonXmlModule;
import com.fasterxml.jackson.dataformat.xml.XmlMapper;
import fr.abes.periscope.core.entity.visualisation.NoticeVisu;
import fr.abes.periscope.core.entity.visualisation.Sequence;
import fr.abes.periscope.core.entity.xml.NoticeXml;
import fr.abes.periscope.core.entity.xml.NoticesBibio;
import fr.abes.periscope.core.exception.IllegalPpnException;
import fr.abes.periscope.core.repository.baseXml.NoticesBibioRepository;
import fr.abes.periscope.core.util.NoticeFormatExportMapper;
import fr.abes.periscope.core.util.NoticeMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.sql.SQLException;
import java.util.List;
import java.util.Optional;

@Service
public class HoldingService {
    private NoticeMapper noticeFormatExportmodelMapper;
    private NoticesBibioRepository repository;

    @Autowired
    public HoldingService(NoticeMapper mapper, NoticesBibioRepository repository) {
        this.noticeFormatExportmodelMapper = mapper;
        this.repository = repository;
    }

    /**
     * Méthode permettant de récupérer une notice de la base xml et de la transformer au format notice + exemplaires + états de collection (toutes séquences incluses)
     * @param ppn ppn de la notice à récupérer
     * @return notice + exemplaires + états de collection
     */
    public NoticeVisu getNoticeWithHoldings(String ppn) throws SQLException, IOException, IllegalPpnException {
        NoticeXml noticeXml = getFirstPpn(ppn);
        return noticeFormatExportmodelMapper.map(noticeXml, NoticeVisu.class);
    }

    private NoticeXml getFirstPpn(String ppn) throws SQLException, IOException {
        Optional<NoticesBibio> noticesBibio = repository.findFirstByPpn(ppn);
        if (noticesBibio.isPresent()) {
            JacksonXmlModule module = new JacksonXmlModule();
            module.setDefaultUseWrapper(false);
            XmlMapper xmlMapper = new XmlMapper(module);
            return xmlMapper.readValue(noticesBibio.get().getDataXml().getCharacterStream(), NoticeXml.class);
        }
        throw new IllegalPpnException("le PPN " + ppn + "n'existe pas");
    }
}
