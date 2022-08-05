package fr.abes.periscope.core.service;

import fr.abes.periscope.core.entity.xml.NoticeXml;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

@Service
@Slf4j
public class NoticeXmlService {
    public boolean isRessourceContinue(NoticeXml notice) {
        String leader = notice.getLeader();
        if (leader.startsWith("d0", 7) || leader.startsWith("s0", 7) || leader.startsWith("i0", 7)) {
            return true;
        }
        return false;
    }
}
