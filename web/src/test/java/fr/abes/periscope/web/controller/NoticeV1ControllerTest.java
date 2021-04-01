package fr.abes.periscope.web.controller;

import fr.abes.periscope.web.PeriscopeApplicationTest;
import org.junit.Assert;
import org.junit.Test;
import org.junit.jupiter.api.DisplayName;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.springframework.http.MediaType;
import org.springframework.test.context.junit4.SpringRunner;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@RunWith(SpringRunner.class)
public class NoticeV1ControllerTest extends PeriscopeApplicationTest {

    @InjectMocks
    protected NoticeV1Controller controller;

    public void contextLoads() {
        Assert.assertNotNull(controller);
    }

    /**
     * Test la route /api/v1/notice/findByCriteria avec la méthode GET
     * @throws Exception
     */
    @Test
    @DisplayName("GET findByCriteria")
    public void findByCriteriaGetMethod() throws Exception {

        mockMvc.perform(get("/api/v1/notice/findByCriteria"))
                .andExpect(status().isMethodNotAllowed())
                .andExpect(jsonPath("$.status").value("METHOD_NOT_ALLOWED"))
                .andExpect(jsonPath("$.timestamp").isNotEmpty())
                .andExpect(jsonPath("$.message").value("Method is not supported for this request"))
                .andExpect(jsonPath("$.debugMessage").exists());
    }

    /**
     * Test la route /api/v1/notice/findByCriteria avec la méthode POST sans paramètres
     * @throws Exception
     */
    @Test
    @DisplayName("POST findByCriteria - sans paramètres")
    public void findByCriteriaPostMethodWithoutParameters() throws Exception {
       String json = "\n" +
               "\n" +
               "{\n" +
               "    \"criteres\":\n" +
               "    [\n" +
               "        {\"type\":\"CriterionPcp\",\"bloc_operator\":\"OU\",\"pcp\":[\"PCAM\"],\"pcp_operator\":[\"ET\"]},\n" +
               "        {\"type\":\"CriterionRcr\",\"bloc_operator\":\"ET\",\"rcr\":[\"341725201\"],\"rcr_operator\":[\"ET\"]}\n" +
               "    ],\n" +
               "    \"tri\":\n" +
               "    [\n" +
               "        {\"sort\":\"EDITOR\",\"order\":\"DESC\"},\n" +
               "        {\"sort\":\"KEY_TITLE\",\"order\":\"ASC\"}\n" +
               "    ]\n" +
               "}\n";

        mockMvc.perform(post("/api/v1/notice/findByCriteria")
                .contentType(MediaType.APPLICATION_JSON).content(json))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.status").value("BAD_REQUEST"))
                .andExpect(jsonPath("$.timestamp").isNotEmpty())
                .andExpect(jsonPath("$.message").value("Missing request parameter"))
                .andExpect(jsonPath("$.debugMessage").exists());
    }

    /**
     * Test la route /api/v1/notice/findByCriteria avec la méthode POST et
     * un JSON vide
     * @throws Exception
     */
    @Test
    @DisplayName("POST findByCriteria - JSON vide")
    public void findByCriteriaPostMethodEmptyJSON() throws Exception {
        String json = "";

        mockMvc.perform(post("/api/v1/notice/findByCriteria?page=0&size=25")
                .contentType(MediaType.APPLICATION_JSON).content(json))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.status").value("BAD_REQUEST"))
                .andExpect(jsonPath("$.timestamp").isNotEmpty())
                .andExpect(jsonPath("$.message").value("Malformed JSON request"))
                .andExpect(jsonPath("$.debugMessage").exists());
    }

    /**
     * Test la route /api/v1/notice/findByCriteria avec la méthode POST et
     * un mauvais JSON (sans critères)
     * @throws Exception
     */
    @Test
    @DisplayName("POST findByCriteria - mauvais JSON - sans critères")
    public void findByCriteriaPostMethodWrongJSON1() throws Exception {
        String json = "{\n" +
                "    \"crites\":\n" +
                "    [\n" +
                "        {\"sort\":\"EDITOR\",\"order\":\"DESC\"},\n" +
                "        {\"sort\":\"KEY_TITLE\",\"order\":\"ASC\"}\n" +
                "    ]\n" +
                "}";

        mockMvc.perform(post("/api/v1/notice/findByCriteria?page=0&size=25")
                .contentType(MediaType.APPLICATION_JSON).content(json))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.status").value("BAD_REQUEST"))
                .andExpect(jsonPath("$.timestamp").isNotEmpty())
                .andExpect(jsonPath("$.message").value("The credentials are not valid"))
                .andExpect(jsonPath("$.debugMessage").exists());
    }

    /**
     * Test la route /api/v1/notice/findByCriteria avec la méthode POST et
     * un mauvais JSON (sans critères)
     * @throws Exception
     */
    @Test
    @DisplayName("POST findByCriteria - mauvais JSON - type")
    public void findByCriteriaPostMethodWrongJSON2() throws Exception {
        String json = "{\n" +
                "    \"criteres\":\n" +
                "    [\n" +
                "        {\"typedd\":\"CriterionPcp\",\"bloc_operator\":\"OU\",\"pcp\":[\"PCAM\"],\"pcp_operator\":[\"ET\"]}\n" +
                "    ],\n" +
                "    \"tri\":\n" +
                "    [\n" +
                "        {\"sort\":\"EDITOR\",\"order\":\"DESC\"},\n" +
                "        {\"sort\":\"KEY_TITLE\",\"order\":\"ASC\"}\n" +
                "    ]\n" +
                "}";

        mockMvc.perform(post("/api/v1/notice/findByCriteria?page=0&size=25")
                .contentType(MediaType.APPLICATION_JSON).content(json))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.status").value("BAD_REQUEST"))
                .andExpect(jsonPath("$.timestamp").isNotEmpty())
                .andExpect(jsonPath("$.message").value("Malformed JSON request"))
                .andExpect(jsonPath("$.debugMessage").exists());
    }

    /**
     * Test la route /api/v1/notice/findByCriteria avec la méthode POST et
     * un mauvais JSON (sans critères)
     * @throws Exception
     */
    @Test
    @DisplayName("POST findByCriteria - mauvais JSON - mauvais type")
    public void findByCriteriaPostMethodWrongJSON3() throws Exception {
        String json = "{\n" +
                "    \"criteres\":\n" +
                "    [\n" +
                "        {\"type\":\"CriterionTOTO\",\"bloc_operator\":\"OU\",\"pcp\":[\"PCAM\"],\"pcp_operator\":[\"ET\"]}\n" +
                "    ],\n" +
                "    \"tri\":\n" +
                "    [\n" +
                "        {\"sort\":\"EDITOR\",\"order\":\"DESC\"},\n" +
                "        {\"sort\":\"KEY_TITLE\",\"order\":\"ASC\"}\n" +
                "    ]\n" +
                "}";

        mockMvc.perform(post("/api/v1/notice/findByCriteria?page=0&size=25")
                .contentType(MediaType.APPLICATION_JSON).content(json))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.status").value("BAD_REQUEST"))
                .andExpect(jsonPath("$.timestamp").isNotEmpty())
                .andExpect(jsonPath("$.message").value("Malformed JSON request"))
                .andExpect(jsonPath("$.debugMessage").exists());
    }

    /**
     * Test la route /api/v1/notice/findByCriteria avec la méthode POST et
     * un mauvais JSON (sans critères)
     * @throws Exception
     */
    @Test
    @DisplayName("POST findByCriteria - mauvais JSON - bloc_operator")
    public void findByCriteriaPostMethodWrongJSON4() throws Exception {
        String json = "{\n" +
                "    \"criteres\":\n" +
                "    [\n" +
                "        {\"type\":\"CriterionTOTO\",\"bloc_opator\":\"OU\",\"pcp\":[\"PCAM\"],\"pcp_operator\":[\"ET\"]}\n" +
                "    ],\n" +
                "    \"tri\":\n" +
                "    [\n" +
                "        {\"sort\":\"EDITOR\",\"order\":\"DESC\"},\n" +
                "        {\"sort\":\"KEY_TITLE\",\"order\":\"ASC\"}\n" +
                "    ]\n" +
                "}";

        mockMvc.perform(post("/api/v1/notice/findByCriteria?page=0&size=25")
                .contentType(MediaType.APPLICATION_JSON).content(json))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.status").value("BAD_REQUEST"))
                .andExpect(jsonPath("$.timestamp").isNotEmpty())
                .andExpect(jsonPath("$.message").value("Malformed JSON request"))
                .andExpect(jsonPath("$.debugMessage").exists());
    }

    /**
     * Test la route /api/v1/notice/findByCriteria avec la méthode POST et
     * un mauvais JSON
     * @throws Exception
     */
    @Test
    @DisplayName("POST findByCriteria - bon JSON")
    public void findByCriteriaPostMethodGoodJSON() throws Exception {
        String json = "{\n" +
                "    \"criteres\":\n" +
                "    [\n" +
                "        {\"type\":\"CriterionPcp\",\"bloc_operator\":\"OU\",\"pcp\":[\"PCAM\"],\"pcp_operator\":[\"ET\"]}\n" +
                "    ],\n" +
                "    \"tri\":\n" +
                "    [\n" +
                "        {\"sort\":\"EDITOR\",\"order\":\"DESC\"},\n" +
                "        {\"sort\":\"KEY_TITLE\",\"order\":\"ASC\"}\n" +
                "    ]\n" +
                "}";

        mockMvc.perform(post("/api/v1/notice/findByCriteria?page=0&size=25")
                .contentType(MediaType.APPLICATION_JSON).content(json))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$").isArray())
                .andExpect(jsonPath("$.[0].ppn").exists())
                .andExpect(jsonPath("$.[0].ppn").isString())
                .andExpect(jsonPath("$.[0].issn").exists())
                .andExpect(jsonPath("$.[0].issn").isString());
    }
}
