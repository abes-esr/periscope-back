package fr.abes.periscope.web.security;


import fr.abes.periscope.web.exception.InvalidTokenException;
import io.jsonwebtoken.*;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import javax.servlet.http.HttpServletRequest;

@Component
@Slf4j
public class JwtTokenProvider {
    @Value("${jwt.secret}")
    private String secret;

    public String generateToken() {
        return "eyJhbGciOiJIUzI1NiJ9.eyJVc2VybmFtZSI6IlBlcmlzY29wZUtleSIsImV4cCI6NDA5MjY0Mzk0OH0.-ENxl1qIUyB9ZsT7CCgIX-YvrKR0R9FGFa5izzH46TI";
    }

    public boolean validateToken(String authToken) {
        try {
            Jwts.parser().setSigningKey(secret).parseClaimsJws(authToken);
            return true;
        } catch (Exception ex) {
            log.error("Erreur d'authentification", ex.getMessage());
        }
        return false;
    }

    public String getJwtFromRequest(HttpServletRequest request) {
        String bearerToken = request.getHeader("Authorization");
        if (StringUtils.hasText(bearerToken) && bearerToken.startsWith("Bearer ")) {
            return bearerToken.substring(7);
        }
        return null;
    }

    public String getUsernameFromJwtToken(String token) {
        return Jwts.parser().setSigningKey(secret).parseClaimsJws(token).getBody().getSubject();
    }
}
