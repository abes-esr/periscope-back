package fr.abes.periscope.web.controller;

import fr.abes.periscope.core.entity.solr.Notice;
import fr.abes.periscope.core.entity.solr.v2.FacetteSolr;
import fr.abes.periscope.core.entity.solr.v2.NoticeV2;
import fr.abes.periscope.core.entity.solr.v2.ResultSolr;
import fr.abes.periscope.core.service.NoticeStoreService;
import fr.abes.periscope.web.PeriscopeApplicationTest;
import fr.abes.periscope.web.util.DtoMapper;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@ExtendWith(SpringExtension.class)
class NoticeV2ControllerTest extends PeriscopeApplicationTest {
    @InjectMocks
    protected NoticeV2Controller controller;

    @MockBean
    private NoticeStoreService service;

    @Autowired
    private DtoMapper mapper;

    @Test
    @DisplayName("test WS avec facettes")
    void testWsWithFacets() throws Exception {
        String json =
                "{\n" +
                "    \"criteres\":\n" +
                "    [\n" +
                "        {\"type\":\"CriterionPcp\",\"bloc_operator\":\"OU\",\"pcp\":[\"PCAM\"],\"pcp_operator\":[\"ET\"]},\n" +
                "        {\"type\":\"CriterionRcr\",\"bloc_operator\":\"ET\",\"rcr\":[\"730512301\"],\"rcr_operator\":[\"ET\"]}\n" +
                "    ],\n" +
                "    \"tri\":\n" +
                "    [\n" +
                "        {\"sort\":\"TITLE\",\"order\":\"ASC\"},\n" +
                "        {\"sort\":\"EDITOR\",\"order\":\"DESC\"},\n" +
                "        {\"sort\":\"NB_LOC\",\"order\":\"DESC\"}\n" +
                "    ],\n" +
                "   \"facettes\":\n" +
                "   [\n" +
                "       {\"zone\":\"DOCUMENT_TYPE\"},\n" +
                "       {\"zone\":\"NB_LOC\"},\n" +
                "       {\"zone\":\"COUNTRY\"}\n" +
                "   ]\n" +
                "}\n";

        ResultSolr result = new ResultSolr();
        Notice notice = new NoticeV2();
        notice.setPpn("111111111");
        notice.setNbLocation(1);
        FacetteSolr facette = new FacetteSolr("LANGUAGE");
        result.setNbNotices(1);
        result.setNbPages(1);
        result.addNotice(notice);
        result.addFacette(facette);

        Mockito.when(service.findNoticesWithFacets(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.anyInt(), Mockito.anyInt())).thenReturn(result);

        this.mockMvc.perform(post("/api/v2/notice/findByCriteriaWithFacets?page=0&size=10")
                .contentType(MediaType.APPLICATION_JSON).content(json))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.notice").isNotEmpty())
                .andExpect(jsonPath("$.facettes").isNotEmpty())
                .andExpect(jsonPath("$.nbPages").isNumber())
                .andExpect(jsonPath("$.nbNotices").isNumber());

    }

    @Test
    @DisplayName("test WS avec facettes & filtres facettes")
    void testWsWithFacetsFilters() throws Exception {
        String json = "\n" +
                "\n" +
                "{\n" +
                "    \"criteres\":\n" +
                "    [\n" +
                "        {\"type\":\"CriterionPcp\",\"bloc_operator\":\"OU\",\"pcp\":[\"PCAM\"],\"pcp_operator\":[\"ET\"]},\n" +
                "        {\"type\":\"CriterionRcr\",\"bloc_operator\":\"ET\",\"rcr\":[\"730512301\"],\"rcr_operator\":[\"ET\"]}\n" +
                "    ],\n" +
                "    \"tri\":\n" +
                "    [\n" +
                "        {\"sort\":\"TITLE\",\"order\":\"ASC\"},\n" +
                "        {\"sort\":\"EDITOR\",\"order\":\"DESC\"},\n" +
                "        {\"sort\":\"NB_LOC\",\"order\":\"DESC\"}\n" +
                "    ],\n" +
                "   \"facettes\":\n" +
                "   [\n" +
                "       {\"zone\":\"DOCUMENT_TYPE\"},\n" +
                "       {\"zone\":\"NB_LOC\"},\n" +
                "       {\"zone\":\"COUNTRY\"}\n" +
                "   ],\n" +
                "   \"filtresFacettes\":\n" +
                "   [\n" +
                "       {\"zone\":\"COUNTRY\",\"valeurs\":[\"FR\",\"US\"]},\n" +
                "       {\"zone\":\"LANGUAGE\",\"valeurs\":[\"fre\"]}\n" +
                "   ]\n" +
                "}\n";

        ResultSolr result = new ResultSolr();
        Notice notice = new NoticeV2();
        notice.setPpn("111111111");
        notice.setNbLocation(1);
        FacetteSolr facette = new FacetteSolr("LANGUAGE");
        result.setNbNotices(1);
        result.setNbPages(1);
        result.addNotice(notice);
        result.addFacette(facette);

        Mockito.when(service.findNoticesWithFacets(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.anyInt(), Mockito.anyInt())).thenReturn(result);

        this.mockMvc.perform(post("/api/v2/notice/findByCriteriaWithFacets?page=0&size=10")
                .contentType(MediaType.APPLICATION_JSON).content(json))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.notice").isNotEmpty())
                .andExpect(jsonPath("$.facettes").isNotEmpty())
                .andExpect(jsonPath("$.nbPages").isNumber())
                .andExpect(jsonPath("$.nbNotices").isNumber());

    }


}
