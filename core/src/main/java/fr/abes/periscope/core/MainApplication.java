package fr.abes.periscope.core;

import fr.abes.periscope.core.entities.Notice;
import fr.abes.periscope.core.service.NoticeStoreService;
import org.springframework.boot.ApplicationRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;

import java.util.List;

@SpringBootApplication
public class MainApplication {

    private final NoticeStoreService noticeStoreService;

    public MainApplication(NoticeStoreService noticeStoreService) {
        this.noticeStoreService = noticeStoreService;
    }

    public static void main(String[] args) {
        SpringApplication.run(MainApplication.class, args);
    }

    @Bean
    public ApplicationRunner init() {
        return args -> {

            Notice notice  = noticeStoreService.findByPpn("039513025");
            System.out.println(notice);

            //List<Notice> notices = noticeStoreService.findNoticesByPcp("PCLim",0,25);
            //notices.forEach(a -> System.out.println(a));
        };
    }
}
