package fr.abes.periscope.core.service;

import fr.abes.periscope.core.entities.Notice;
import fr.abes.periscope.core.repository.NoticeRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@Slf4j
public class NoticeStoreService {

    private final NoticeRepository noticeRepository;

    @Autowired
    public NoticeStoreService(NoticeRepository noticeRepository) {
        this.noticeRepository = noticeRepository;
    }   

    public Notice findByPpn(String code) {
        return noticeRepository.findByPpn(code);
    }

    public List<Notice> findNoticesByPcp(String code, int page, int size) {
        return noticeRepository.findNoticesByPcp(code, PageRequest.of(page, size,
                Sort.by(Sort.Direction.ASC, "ppn")));
    }
}
