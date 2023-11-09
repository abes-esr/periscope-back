package fr.abes.periscope.core;

import com.fasterxml.jackson.dataformat.xml.JacksonXmlModule;
import com.fasterxml.jackson.dataformat.xml.XmlMapper;
import fr.abes.periscope.core.entity.solr.NoticeSolr;
import fr.abes.periscope.core.entity.xml.NoticeXml;
import fr.abes.periscope.core.util.UtilsMapper;
import org.apache.commons.io.IOUtils;
import org.springframework.core.io.Resource;

import java.io.FileInputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;

public class TestUtils {
    public static NoticeSolr getNoticeFromFile(Resource file, UtilsMapper noticeMapper) throws IOException {
        String xml = IOUtils.toString(new FileInputStream(file.getFile()), StandardCharsets.UTF_8);

        JacksonXmlModule module = new JacksonXmlModule();
        module.setDefaultUseWrapper(false);
        XmlMapper xmlMapper = new XmlMapper(module);
        NoticeXml notice = xmlMapper.readValue(xml, NoticeXml.class);
        return noticeMapper.map(notice, NoticeSolr.class);
    }
}
