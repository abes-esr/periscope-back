package fr.abes.periscope.web.controller;

import fr.abes.periscope.PeriscopeApplication;
import fr.abes.periscope.core.entity.Notice;
import fr.abes.periscope.core.service.NoticeStoreService;
import fr.abes.periscope.web.util.DtoMapper;
import org.aspectj.weaver.ast.Not;
import org.junit.Before;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.http.MediaType;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@RunWith(SpringRunner.class)
@AutoConfigureMockMvc
@SpringBootTest(classes = PeriscopeApplication.class)
public class PublicControllerTest {
    @InjectMocks
    protected PublicController publicController;

    @Autowired
    MockMvc mockMvc;

    @MockBean
    NoticeStoreService service;

    @Autowired
    DtoMapper mapper;

    @Before
    public void setup(){
        publicController = new PublicController(service, mapper);
    }

    Notice notice;
    @BeforeEach
    void init() {
        notice = new Notice();
        notice.setPpn("123456789");
        notice.setIssn("1234-5678");
        HashSet<String> pcpHash = new HashSet<>();
        pcpHash.add("PCCor");
        notice.setPcpList(pcpHash);
        HashSet<String> rcrHash = new HashSet<>();
        rcrHash.add("341725201");
    }

    @Test
    void testSort() throws Exception {
        List<Notice> notices = new ArrayList<>();
        notices.add(notice);
        given(this.service.findNoticesByCriteria(any(), any(), 0, 25)).willReturn(notices);
        this.mockMvc.perform(post("/api/notice/findByCriteria?page=0&size=25").accept(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andExpect(jsonPath("$.ppn").value("12345678"));
    }
}
