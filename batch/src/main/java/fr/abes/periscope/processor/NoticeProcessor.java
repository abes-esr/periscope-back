package fr.abes.periscope.processor;

import com.fasterxml.jackson.dataformat.xml.JacksonXmlModule;
import com.fasterxml.jackson.dataformat.xml.XmlMapper;
import fr.abes.periscope.core.entity.solr.NoticeSolr;
import fr.abes.periscope.core.entity.xml.NoticeXml;
import fr.abes.periscope.core.entity.xml.NoticesBibio;
import fr.abes.periscope.core.service.NoticeXmlService;
import fr.abes.periscope.core.util.UtilsMapper;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.batch.item.ItemProcessor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;


@Component
@Getter @Setter
@Slf4j
public class NoticeProcessor implements ItemProcessor<NoticesBibio, NoticeSolr> {
    private String threadName;

    @Autowired
    private UtilsMapper noticeMapper;

    @Autowired
    private NoticeXmlService service;

    @Override
    public NoticeSolr process(NoticesBibio notice) throws Exception {
        log.debug("Processing " + threadName + " : notice n°" + notice.getId());
        try {
            JacksonXmlModule module = new JacksonXmlModule();
            module.setDefaultUseWrapper(false);
            XmlMapper xmlMapper = new XmlMapper(module);
            NoticeXml noticeXml = xmlMapper.readValue(notice.getDataXml().getCharacterStream(), NoticeXml.class);
            if (service.isRessourceContinue(noticeXml)) {
                return noticeMapper.map(noticeXml, NoticeSolr.class);
            }
        }catch (Exception ex) {
            log.error("Erreur dans la conversion JSON notice n° : " + notice.getId() + " Exception " + ex.getClass().getName() + " : " + ex.getMessage());
        }
        return null;
    }
}
