package fr.abes.periscope.core;

import org.springframework.test.annotation.IfProfileValue;
import org.springframework.test.context.junit.jupiter.EnabledIf;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Target({ ElementType.TYPE, ElementType.METHOD })
@Retention(RetentionPolicy.RUNTIME)
@EnabledIf(expression = "#{environment['spring.profiles.active'] == 'test-solr'}", loadContext = true)
@IfProfileValue(name ="spring.profiles.active", value ="test-solr")
public @interface EnableOnIntegrationTest {}