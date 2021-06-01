package fr.abes.periscope.web.controller;

import fr.abes.periscope.web.PeriscopeApplicationTest;
import org.junit.Assert;
import org.junit.Test;
import org.junit.jupiter.api.DisplayName;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.springframework.http.MediaType;
import org.springframework.test.context.junit4.SpringRunner;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@RunWith(SpringRunner.class)
public class NoticeV2ControllerTest extends PeriscopeApplicationTest {
    @InjectMocks
    protected NoticeV2Controller controller;

    public void contextLoads() {
        Assert.assertNotNull(controller);
    }

    @Test
    @DisplayName("test WS avec facettes")
    public void testWsWithFacets() throws Exception {
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
                "   ]\n" +
                "}\n";

        this.mockMvc.perform(post("/api/v2/notice/findByCriteriaWithFacets?page=0&size=10")
                .contentType(MediaType.APPLICATION_JSON).content(json))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.notice").isNotEmpty())
                .andExpect(jsonPath("$.facettes").isNotEmpty())
                .andExpect(jsonPath("$.nbPages").isNumber())
                .andExpect(jsonPath("$.nbNotices").isNumber());

    }


}
