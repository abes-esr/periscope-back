package fr.abes.periscope.core;

import fr.abes.periscope.core.service.NoticeStoreService;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

@SpringBootApplication
public class MainApplication {

    private final NoticeStoreService noticeStoreService;

    public MainApplication(NoticeStoreService noticeStoreService) {
        this.noticeStoreService = noticeStoreService;
    }

    public static void main(String[] args) {
        SpringApplication.run(MainApplication.class, args);
    }
}
