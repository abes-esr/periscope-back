package fr.abes.periscope.core.configuration;

import fr.abes.periscope.core.repository.solr.SolrQueryBuilder;
import org.apache.solr.client.solrj.SolrClient;
import org.apache.solr.client.solrj.impl.HttpSolrClient;
import org.apache.solr.client.solrj.impl.XMLResponseParser;
import org.apache.solr.common.params.ModifiableSolrParams;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.solr.core.SolrTemplate;

/**
 * Configuration du client SolR
 */
@Configuration
public class SolRConfig {

    @Bean
    public SolrClient solrClient() {
        ModifiableSolrParams params = new ModifiableSolrParams();
        params.add("solrService","Pcp");
        params.add("wt", "xml");
        params.add("version","2.2");
        params.add("indent", "on");
        params.add("omitHeader","true");

        HttpSolrClient.Builder builder = new HttpSolrClient.Builder()
                .withBaseSolrUrl("https://periscope.sudoc.fr/SolrProxy")
                .withInvariantParams(params)
                .withResponseParser(new QESXMLResponseParser());
        return builder.build();
    }

    @Bean
    public SolrTemplate solrTemplate(SolrClient client) throws Exception {
        SolrTemplate template = new SolrTemplate(client);
        return template;
    }

    protected class QESXMLResponseParser extends XMLResponseParser {
        public QESXMLResponseParser() { super(); }

        @Override
        public String getContentType() {
            return "text/xml; charset=UTF-8";
        }
    }

    @Bean
    public SolrQueryBuilder builderQuery() {
        return new SolrQueryBuilder();
    }
}


