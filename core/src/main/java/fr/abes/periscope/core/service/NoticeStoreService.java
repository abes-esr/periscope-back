package fr.abes.periscope.core.service;

import fr.abes.periscope.core.entity.Notice;
import fr.abes.periscope.core.entity.NoticeSolr;
import fr.abes.periscope.core.repository.NoticeRepository;
import fr.abes.periscope.core.util.NoticeMapper;
import fr.abes.periscope.core.util.TrackExecutionTime;
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

    private final NoticeMapper noticeMapper;

    @Autowired
    public NoticeStoreService(NoticeRepository noticeRepository, NoticeMapper mapper) {
        this.noticeRepository = noticeRepository;
        this.noticeMapper = mapper;
    }

    @TrackExecutionTime
    public Notice findByPpn(String code) {
        NoticeSolr notice = noticeRepository.findByPpn(code);
        return noticeMapper.map(notice);
    }

    @TrackExecutionTime
    public List<Notice> findNoticesByPcp(String code, int page, int size) {
        List<NoticeSolr> notices = noticeRepository.findNoticesByPcp(code, PageRequest.of(page, size,
                Sort.by(Sort.Direction.ASC, "ppn")));
        return noticeMapper.mapList(notices);
    }

    @TrackExecutionTime
    public List<Notice> findNoticesByPcpComplex(String code, int page, int size) {
        List<NoticeSolr> notices = noticeRepository.findNoticesByMultiplePcp(code, PageRequest.of(page, size,
                Sort.by(Sort.Direction.ASC, "ppn")));
        return noticeMapper.mapList(notices);
    }

    @TrackExecutionTime
    public List<Notice> findNoticesByMultipleCriterion(String containsValue,int page, int size) {
        List<NoticeSolr> notices = noticeRepository.findNoticesByMultipleCriterion(containsValue,PageRequest.of(page, size,
                Sort.by(Sort.Direction.ASC, "ppn")));
        return noticeMapper.mapList(notices);
    }
}
