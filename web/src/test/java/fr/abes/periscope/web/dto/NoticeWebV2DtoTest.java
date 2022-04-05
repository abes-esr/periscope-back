package fr.abes.periscope.web.dto;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class NoticeWebV2DtoTest {
    @Test
    public void testGetTitre() {
        NoticeWebV2Dto notice = new NoticeWebV2Dto();
        notice.setTitreCle("titre clé");
        assertEquals(notice.getTitre(), "titre clé");
        notice.setTitreCleQualifie("titre clé qualifié");
        assertEquals(notice.getTitre(), "titre clé titre clé qualifié");

        notice = new NoticeWebV2Dto();
        notice.setTitreCleCourt("titre clé court");
        assertEquals(notice.getTitre(), "titre clé court");

        notice = new NoticeWebV2Dto();
        notice.setTitrePropre("titre propre");
        assertEquals(notice.getTitre(), "titre propre");

        notice = new NoticeWebV2Dto();
        notice.setTitreAuteurDifferent("titre d'un auteur différent");
        assertEquals(notice.getTitre(), "titre d'un auteur différent");

        notice = new NoticeWebV2Dto();
        notice.setTitreParallele("titre parallèle");
        assertEquals(notice.getTitre(), "titre parallèle");

        notice = new NoticeWebV2Dto();
        notice.setTitreComplement("Complément de titre");
        assertEquals(notice.getTitre(), "Complément de titre");
    }
}
