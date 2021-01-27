package fr.abes.periscope.core;

import fr.abes.periscope.core.entity.Notice;
import fr.abes.periscope.core.service.NoticeStoreService;
import org.apache.commons.math3.stat.descriptive.summary.Product;
import org.springframework.boot.ApplicationRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;

import java.util.ArrayList;
import java.util.Arrays;
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


    /*@Bean
    public ApplicationRunner init() {
        return args -> {
            List<String> pcp = Arrays.asList("PCLim","PCAq");
            List<Notice> notices = noticeStoreService.findNoticesByPcp(pcp,0,25);
            notices.forEach(a -> System.out.println(a));
        };
    }*/
}
