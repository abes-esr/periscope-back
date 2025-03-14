package fr.abes.periscope.web.util;

import org.springframework.beans.factory.annotation.Value;

public class Constant {
    @Value("${url.sudoc}")
    public static String SUDOC_URL;
}
