package fr.abes.periscope.core.repository.baseXml;

import fr.abes.periscope.core.entity.xml.NoticesBibio;
import fr.abes.periscope.core.util.BaseXMLConfiguration;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@BaseXMLConfiguration
@Repository
public interface NoticesBibioRepository extends JpaRepository<NoticesBibio, Integer> {
    Optional<NoticesBibio> findFirstByPpn(String ppn);

    @Query(value = "select max(id) from autorites.noticesbibio", nativeQuery = true)
    Integer findMaxId();

    @Query(value = "select min(id) from autorites.noticesbibio", nativeQuery = true)
    Integer findMinId();

}
