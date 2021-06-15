package fr.abes.periscope.web.controller;

import fr.abes.periscope.web.PeriscopeApplicationTest;
import org.junit.Assert;
import org.junit.Test;
import org.junit.jupiter.api.DisplayName;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.springframework.test.context.junit4.SpringRunner;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@RunWith(SpringRunner.class)
public class HoldingsControllerTest extends PeriscopeApplicationTest {
    @InjectMocks
    protected HoldingsController controller;

    public void contextLoads() {
        Assert.assertNotNull(controller);
    }

    @Test
    @DisplayName("test récupération états de collection")
    public void testHolding() throws Exception {
        String ppn = "13282261X";

        this.mockMvc.perform(get("/api/v2/holdings/" + ppn))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.holdings").isNotEmpty());
    }
}
