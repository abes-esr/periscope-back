package fr.abes.periscope;

import fr.abes.periscope.web.security.JwtTokenProvider;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.builder.SpringApplicationBuilder;
import org.springframework.boot.web.servlet.support.SpringBootServletInitializer;
import org.springframework.security.authentication.AnonymousAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;

import java.util.ArrayList;
import java.util.List;

@SpringBootApplication
@Slf4j
public class PeriscopeAPIApplication extends SpringBootServletInitializer implements CommandLineRunner {
	@Value("${jwt.anonymousUser")
	private String user;
	@Autowired
	private JwtTokenProvider tokenProvider;

	public static void main(String[] args) {
		SpringApplication.run(PeriscopeAPIApplication.class, args);
	}

	@Override
	protected SpringApplicationBuilder configure(SpringApplicationBuilder builder) {
		return builder.sources(PeriscopeAPIApplication.class);
	}

	@Override
	public void run(String... args) throws Exception {
		List<GrantedAuthority> roles = new ArrayList<>();
		roles.add(new SimpleGrantedAuthority("ANONYMOUS"));
		String token = tokenProvider.generateToken();
		log.info("token : " + token);
		Authentication auth = new AnonymousAuthenticationToken(token, user, roles);
		SecurityContextHolder.getContext().setAuthentication(auth);
	}
}
