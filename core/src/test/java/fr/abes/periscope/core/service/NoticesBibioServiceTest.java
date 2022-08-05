package fr.abes.periscope.core.service;

import fr.abes.periscope.core.CoreTestConfiguration;
import fr.abes.periscope.core.EnableOnIntegrationTest;
import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

@EnableOnIntegrationTest
@SpringBootTest(classes = {CoreTestConfiguration.class})
public class NoticesBibioServiceTest {

    @Autowired
    NoticesBibioService service;

    @Test
    void testFindById() {
        Assertions.assertThat(service.findById(1234567).getId()).isEqualTo(1234567);
    }

}
