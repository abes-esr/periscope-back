package fr.abes.periscope.web;

import fr.abes.periscope.PeriscopeAPIApplication;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.web.servlet.MockMvc;

@AutoConfigureMockMvc
@SpringBootTest(classes = PeriscopeAPIApplication.class)
public class PeriscopeApplicationTest {

    @Autowired
    protected MockMvc mockMvc;
}
