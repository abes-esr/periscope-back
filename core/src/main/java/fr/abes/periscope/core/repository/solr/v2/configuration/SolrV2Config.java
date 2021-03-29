package fr.abes.periscope.core.repository.solr.v2.configuration;

import fr.abes.periscope.core.repository.solr.v2.SolrQueryBuilder;
import org.apache.solr.client.solrj.SolrClient;
import org.apache.solr.client.solrj.impl.HttpSolrClient;
import org.apache.solr.client.solrj.impl.XMLResponseParser;
import org.apache.solr.common.params.ModifiableSolrParams;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.solr.core.SolrTemplate;

/**
 * Configuration du client SolR
 */
@Configuration
public class SolrV2Config {

    @Value("${solr.v2.baseurl}")
    private String baseURL;

    @Bean
    public SolrClient solrV2Client() {
        ModifiableSolrParams params = new ModifiableSolrParams();
        params.add("wt", "xml");
        params.add("version","2.2");
        params.add("indent", "on");
        params.add("omitHeader","false");

        HttpSolrClient.Builder builder = new HttpSolrClient.Builder()
                .withBaseSolrUrl(this.baseURL)
                .withInvariantParams(params)
                .withResponseParser(new XMLResponseParser());
        return builder.build();
    }

    @Bean
    @Qualifier("solrV2Template")
    public SolrTemplate solrV2Template() {
        return new SolrTemplate(solrV2Client());
    }

    @Bean
    @Qualifier("SolrQueryV2Builder")
    public SolrQueryBuilder builderV2Query() {
        return new SolrQueryBuilder();
    }
}


