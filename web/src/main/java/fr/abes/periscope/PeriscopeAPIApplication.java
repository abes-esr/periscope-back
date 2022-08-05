package fr.abes.periscope;

import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.builder.SpringApplicationBuilder;
import org.springframework.boot.web.servlet.support.SpringBootServletInitializer;

@SpringBootApplication
@Slf4j
public class PeriscopeAPIApplication extends SpringBootServletInitializer implements CommandLineRunner {


	public static void main(String[] args) {
		SpringApplication.run(PeriscopeAPIApplication.class, args);
	}

	@Override
	protected SpringApplicationBuilder configure(SpringApplicationBuilder builder) {
		return builder.sources(PeriscopeAPIApplication.class);
	}

	@Override
	public void run(String... args) throws Exception {
	}
}
