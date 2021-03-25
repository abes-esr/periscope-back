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
       String json = "[\n" +
               "   {\n" +
               "    \"type\":\"CriterionRcr\",\n" +
               "    \"bloc_operator\":\"SAUF\",   \n" +
               "    \"rcr\":[\"661362104\"],\n" +
               "    \"rcr_operator\":[\"ET\"]   \n" +
               "    }\n" +
               "]";

        mockMvc.perform(post("/api/v1/notice/findByCriteria")
                .contentType(MediaType.APPLICATION_JSON).content(json))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.status").value("BAD_REQUEST"))
                .andExpect(jsonPath("$.timestamp").isNotEmpty())
                .andExpect(jsonPath("$.message").value("Malformed JSON request"))
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
     * un mauvais JSON
     * @throws Exception
     */
    @Test
    @DisplayName("POST findByCriteria - mauvais JSON")
    public void findByCriteriaPostMethodWrongJSON() throws Exception {
        String json = "[\n" +
                "   {\n" +
                "    \"type\":\"CriterionRcr\",\n" +
                "    \"bloc_operator\":\"SAUF\",   \n" +
                "    \"rcr\":[\"661362104\"],\n" +
                "    \"rcr_operator\":[\"ET\"]   \n" +
                "    }\n" +
                "]";

        mockMvc.perform(post("/api/v1/notice/findByCriteria?page=0&size=25")
                .contentType(MediaType.APPLICATION_JSON).content(json))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.status").value("BAD_REQUEST"))
                .andExpect(jsonPath("$.timestamp").isNotEmpty())
                .andExpect(jsonPath("$.message").value("Malformed JSON request"))
                .andExpect(jsonPath("$.debugMessage").exists());
    }
}
