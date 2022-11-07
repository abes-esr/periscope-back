package fr.abes.periscope.core.configuration;

import fr.abes.periscope.core.repository.solr.SolrQueryBuilder;
import lombok.extern.slf4j.Slf4j;
import org.apache.solr.client.solrj.SolrClient;
import org.apache.solr.client.solrj.impl.HttpSolrClient;
import org.apache.solr.client.solrj.impl.XMLResponseParser;
import org.apache.solr.common.SolrException;
import org.apache.solr.common.params.ModifiableSolrParams;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.solr.core.SolrTemplate;
import org.springframework.data.solr.repository.config.EnableSolrRepositories;

/**
 * Configuration du client SolR
 */
@Configuration
@Slf4j
public class SolrConfig {

    @Value("${solr.baseurl}")
    private String baseURL;

    @Bean
    public SolrClient solrClient() {

        if (baseURL.isEmpty()) {
            throw  new SolrException(SolrException.ErrorCode.SERVER_ERROR,"baseURL is empty");
        }
        log.info("url : " + baseURL);
        ModifiableSolrParams params = new ModifiableSolrParams();
        params.add("wt", "xml");
        params.add("version","2.2");
        params.add("indent", "on");
        params.add("omitHeader","true");

        HttpSolrClient.Builder builder = new HttpSolrClient.Builder()
                .withBaseSolrUrl(this.baseURL)
                .withInvariantParams(params)
                .withResponseParser(new XMLResponseParser());
        return builder.build();
    }

    @Bean
    public SolrTemplate solrTemplate() {
        return new SolrTemplate(solrClient());
    }

    @Bean
    public SolrQueryBuilder solrQueryBuilder() {
        return new SolrQueryBuilder();
    }
}


