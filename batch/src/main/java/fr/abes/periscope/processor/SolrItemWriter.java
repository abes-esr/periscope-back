package fr.abes.periscope.processor;

import fr.abes.periscope.core.entity.solr.NoticeSolr;
import fr.abes.periscope.core.service.NoticeStoreService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.batch.item.ItemStreamWriter;
import org.springframework.batch.item.support.AbstractItemStreamItemWriter;
import org.springframework.beans.factory.InitializingBean;

import java.util.ArrayList;
import java.util.List;

@Slf4j
public class SolrItemWriter extends AbstractItemStreamItemWriter<NoticeSolr>
        implements ItemStreamWriter<NoticeSolr>, InitializingBean {
    private NoticeStoreService service;

    public SolrItemWriter(NoticeStoreService noticeStoreService) {
        this.service = noticeStoreService;
    }

    @Override
    public void write(List<? extends NoticeSolr> list) throws Exception {
        long startTime = System.currentTimeMillis();
        List<NoticeSolr> listToSave = new ArrayList<>();
        listToSave.addAll(list);
        service.saveOrDeleteList(listToSave);
        long endTime = System.currentTimeMillis();
        log.debug("Temps d'écriture Solr : " + (endTime - startTime) + "ms");
    }

    @Override
    public void afterPropertiesSet() throws Exception {

    }
}
