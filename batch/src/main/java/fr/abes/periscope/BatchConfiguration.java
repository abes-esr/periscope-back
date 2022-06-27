package fr.abes.periscope;

import fr.abes.periscope.partitioner.RangePartitioner;
import fr.abes.periscope.processor.NoticePagingItemReader;
import fr.abes.periscope.processor.NoticeProcessor;
import fr.abes.periscope.core.entity.solr.NoticeSolr;
import fr.abes.periscope.core.entity.xml.NoticesBibio;
import fr.abes.periscope.processor.SolrItemWriter;
import fr.abes.periscope.core.service.NoticeStoreService;
import fr.abes.periscope.core.service.NoticesBibioService;
import fr.abes.periscope.core.util.NoticesBibioMapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobParametersIncrementer;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.configuration.annotation.*;
import org.springframework.batch.core.partition.PartitionHandler;
import org.springframework.batch.core.partition.support.TaskExecutorPartitionHandler;
import org.springframework.batch.item.database.Order;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.task.SimpleAsyncTaskExecutor;

import javax.persistence.EntityManagerFactory;
import javax.sql.DataSource;
import java.util.HashMap;
import java.util.Map;


@Slf4j
@Configuration
@EnableBatchProcessing
public class BatchConfiguration {
    @Value("${gridSize}")
    private int gridSize;
    @Value("${chunkSize}")
    private int chunkSize;
    @Value("${minValue}")
    private int minValue;
    @Value("${maxValue}")
    private int maxValue;

    @Autowired
    private JobBuilderFactory jobs;
    @Autowired
    private StepBuilderFactory stepBuilderFactory;
    @Autowired
    private EntityManagerFactory baseXmlEntityManager;

    @Autowired
    private NoticesBibioService noticesBibioService;
    @Autowired
    private NoticeStoreService noticeStoreService;

    long startTime;
    long endTime;

    @Autowired
    protected DataSource baseXmlDataSource;

    @Bean
    public BatchConfigurer configurer() {
        return new IndexingBatchConfigurer(baseXmlEntityManager);
    }

    @Bean
    public Job jobIndexerTableNoticesBibio() throws Exception {
        startTime = System.currentTimeMillis();
        return this.jobs.get("indexerTableNoticesBibio").incrementer(incrementer())
                .start(managerStep())
                .build();
    }

    @Bean
    public Step managerStep() throws Exception {
        return stepBuilderFactory.get("managerStep").partitioner(slaveStep().getName(), rangePartitioner())
                .partitionHandler(masterSlaveHandler()).build();
    }

    @Bean
    public PartitionHandler masterSlaveHandler() throws Exception {
        TaskExecutorPartitionHandler handler = new TaskExecutorPartitionHandler();
        handler.setGridSize(gridSize);
        handler.setTaskExecutor(taskExecutor());
        handler.setStep(slaveStep());
        try {
            handler.afterPropertiesSet();
        } catch (Exception e) {
            log.error("Erreur de configuration du partition handler : " + e.getMessage());
        }
        return handler;
    }

    @Bean(name = "slave")
    public Step slaveStep() throws Exception {
        return stepBuilderFactory.get("slave").<NoticesBibio, NoticeSolr>chunk(chunkSize)
                .reader(slaveReader(null, null))
                .processor(slaveProcessor(null)).writer(slaveWriter()).build();
    }

    @Bean
    public RangePartitioner rangePartitioner() {
        return new RangePartitioner(noticesBibioService, minValue, maxValue);
    }

    @Bean
    public SimpleAsyncTaskExecutor taskExecutor() {
        return new SimpleAsyncTaskExecutor();
    }

    @Bean
    @StepScope
    public NoticePagingItemReader<NoticesBibio> slaveReader(@Value("#{stepExecutionContext['minValue']}") Integer minValue,
                                                            @Value("#{stepExecutionContext['maxValue']}") Integer maxValue) throws Exception {
        log.debug("slaveReader start " + minValue + " " + maxValue);
        NoticePagingItemReader reader = new NoticePagingItemReader();
        reader.setDataSource(baseXmlDataSource);
        reader.setFetchSize(chunkSize);
        reader.setQueryProvider(queryProvider());
        Map<String, Integer> parameters = new HashMap<>();
        parameters.put("minValue", minValue);
        parameters.put("maxValue", maxValue);
        reader.setParameterValues(parameters);
        reader.setPageSize(chunkSize);
        reader.setRowMapper(new NoticesBibioMapper());
        reader.setSaveState(true);
        return reader;
    }

    @Bean
    @StepScope
    public NoticeProcessor slaveProcessor(@Value("#{stepExecutionContext[name]}") String name) {
        log.info("Appel au slaveProcessor");
        NoticeProcessor noticeProcessor = new NoticeProcessor();
        noticeProcessor.setThreadName(name);
        return noticeProcessor;
    }

    @Bean
    @StepScope
    public SolrItemWriter slaveWriter() {
        return new SolrItemWriter(noticeStoreService);
    }

    private NoticeQueryProvider queryProvider() throws Exception {
        NoticeQueryProvider provider = new NoticeQueryProvider();
        provider.setSelectClause("id, XMLSERIALIZE (CONTENT data_xml as CLOB) as data_xml");
        provider.setFromClause("autorites.noticesbibio");
        provider.setWhereClause("id >= :minValue and id < :maxValue");
        Map<String, Order> sortKeys = new HashMap<>();
        sortKeys.put("id", Order.ASCENDING);
        provider.setSortKeys(sortKeys);
        return provider;
    }


    protected JobParametersIncrementer incrementer() {
        return new TimeIncrementer();
    }
}
