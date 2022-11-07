package fr.abes.periscope.web;

import fr.abes.periscope.PeriscopeAPIApplication;
import fr.abes.periscope.web.security.SecurityConfigurer;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.web.servlet.handler.HandlerMappingIntrospector;

@AutoConfigureMockMvc
@SpringBootTest(classes = {PeriscopeAPIApplication.class, HandlerMappingIntrospector.class, SecurityConfigurer.class})
public class PeriscopeApplicationTest {

    @Autowired
    protected MockMvc mockMvc;
}
