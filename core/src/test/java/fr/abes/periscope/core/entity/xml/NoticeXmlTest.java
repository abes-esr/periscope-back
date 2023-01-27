package fr.abes.periscope.core.entity.xml;

import com.fasterxml.jackson.dataformat.xml.JacksonXmlModule;
import com.fasterxml.jackson.dataformat.xml.XmlMapper;
import fr.abes.periscope.core.CoreTestConfiguration;
import fr.abes.periscope.core.EnableOnIntegrationTest;
import fr.abes.periscope.core.util.BaseXMLConfiguration;
import org.apache.commons.io.IOUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.core.io.Resource;

import java.io.FileInputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;

@EnableOnIntegrationTest
@SpringBootTest(classes = {CoreTestConfiguration.class})
@ComponentScan(excludeFilters = @ComponentScan.Filter(BaseXMLConfiguration.class))
public class NoticeXmlTest {

    @Value("classpath:noticeXml/13282261X.xml")
    private Resource noticeXml;

    @Test
    void getPpnTest() throws IOException {
        String xml = IOUtils.toString(new FileInputStream(noticeXml.getFile()), StandardCharsets.UTF_8);

        JacksonXmlModule module = new JacksonXmlModule();
        module.setDefaultUseWrapper(false);
        XmlMapper xmlMapper = new XmlMapper(module);
        NoticeXml notice = xmlMapper.readValue(xml, NoticeXml.class);

        Assertions.assertEquals("13282261X", notice.getPpn());
    }

    @Test
    void toStringTest() throws IOException {
        String xml = IOUtils.toString(new FileInputStream(noticeXml.getFile()), StandardCharsets.UTF_8);

        JacksonXmlModule module = new JacksonXmlModule();
        module.setDefaultUseWrapper(false);
        XmlMapper xmlMapper = new XmlMapper(module);
        NoticeXml notice = xmlMapper.readValue(xml, NoticeXml.class);

        Assertions.assertEquals("Notice {"+ "leader=cas0 22 450, ppn=13282261X}", notice.toString());
    }
}
