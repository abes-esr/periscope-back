package fr.abes.periscope.web.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import fr.abes.periscope.core.entity.solr.PublicationYear;
import fr.abes.periscope.core.entity.visualisation.*;
import fr.abes.periscope.core.exception.IllegalPpnException;
import fr.abes.periscope.core.service.HoldingService;
import fr.abes.periscope.core.util.UtilsMapper;
import fr.abes.periscope.web.PeriscopeApplicationTest;
import fr.abes.periscope.web.dto.HoldingWebDto;
import fr.abes.periscope.web.dto.NoticeVisuWebDto;
import fr.abes.periscope.web.dto.SequenceWebDto;
import fr.abes.periscope.web.security.SecurityConfigurer;
import fr.abes.periscope.web.util.DtoMapper;
import fr.abes.periscope.web.util.TYPE_SEQUENCE;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.Calendar;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@ExtendWith(SpringExtension.class)
class HoldingsControllerTest extends PeriscopeApplicationTest {
    @InjectMocks
    protected HoldingsController controller;

    @MockBean
    private HoldingService service;

    @Autowired
    private ObjectMapper mapper;

    @Test
    @DisplayName("test récupération états de collection")
    void testHolding() throws Exception {
        String ppn = "13282261X";

        NoticeVisu notice = new NoticeVisu();
        notice.setStartYear(new PublicationYear("2010", 0));
        notice.setEndYear(new PublicationYear("2022", 0));

        Holding holding1 = new Holding("999999999");
        notice.addHolding(holding1);

        Mockito.when(service.getNoticeWithHoldings(ppn)).thenReturn(notice);

        this.mockMvc.perform(get("/api/v2/holdings/" + ppn))
                .andExpect(status().isOk());
    }

    @Test
    @DisplayName("test levée exception en cas de PPN inconnu")
    void testHoldingWrongPpn() throws Exception {
        String ppn = "987654321";

        Mockito.when(service.getNoticeWithHoldings("987654321")).thenThrow(new IllegalPpnException("le PPN " + ppn + " n'existe pas"));

        this.mockMvc.perform(get("/api/v2/holdings/" + ppn))
                .andExpect(status().isBadRequest());
    }
}
