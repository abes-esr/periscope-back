package fr.abes.periscope.core;

import fr.abes.periscope.core.criterion.Criterion;
import fr.abes.periscope.core.criterion.CriterionPcp;
import fr.abes.periscope.core.entity.Notice;
import fr.abes.periscope.core.service.NoticeStoreService;
import org.springframework.boot.ApplicationRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;

import java.util.Arrays;
import java.util.LinkedList;
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
            List<Criterion> criteria = new LinkedList<>();

            List<String> pcp = Arrays.asList("PCCor","PCPACA");
            List<String> pcpOperator = Arrays.asList("ET","ET");
            CriterionPcp criterionPcp = new CriterionPcp("OU",pcp,pcpOperator);

            criteria.add(criterionPcp);
            List<Notice> notices = noticeStoreService.findNoticesByCriteria(criteria,0,25);
            notices.forEach(a -> System.out.println(a));
        };
    }
}
