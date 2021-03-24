package fr.abes.periscope;

import fr.abes.periscope.core.repository.NoticeV1Repository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.builder.SpringApplicationBuilder;
import org.springframework.boot.web.servlet.support.SpringBootServletInitializer;

@SpringBootApplication
@Slf4j
public class PeriscopeApplication extends SpringBootServletInitializer implements CommandLineRunner {

	@Autowired
	private NoticeV1Repository noticeRepository;

	public static void main(String[] args) {
		SpringApplication.run(PeriscopeApplication.class, args);
	}

	@Override
	protected SpringApplicationBuilder configure(SpringApplicationBuilder builder) {
		return builder.sources(PeriscopeApplication.class);
	}

	@Override
	public void run(String... args) throws Exception {

		log.trace("Trace Message!");
		log.debug("Debug Message!");
		log.info("Info Message!");
		log.warn("Warn Message!");
		log.error("Error Message!");
	}

}
