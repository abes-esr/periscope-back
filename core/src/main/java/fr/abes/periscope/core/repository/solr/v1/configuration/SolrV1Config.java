package fr.abes.periscope.core.repository.solr.v1.configuration;

import fr.abes.periscope.core.repository.solr.v1.SolrV1QueryBuilder;
import org.apache.solr.client.solrj.SolrClient;
import org.apache.solr.client.solrj.impl.HttpSolrClient;
import org.apache.solr.client.solrj.impl.XMLResponseParser;
import org.apache.solr.common.params.ModifiableSolrParams;
import org.modelmapper.ModelMapper;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.solr.core.SolrTemplate;

/**
 * Configuration du client SolR
 */
@Configuration
public class SolrV1Config {

    @Bean
    public SolrClient solrV1Client() {
        ModifiableSolrParams params = new ModifiableSolrParams();
        params.add("solrService","Pcp");
        params.add("wt", "xml");
        params.add("version","2.2");
        params.add("indent", "on");
        params.add("omitHeader","false");

        HttpSolrClient.Builder builder = new HttpSolrClient.Builder()
                .withBaseSolrUrl("https://periscope.sudoc.fr/SolrProxy")
                .withInvariantParams(params)
                .withResponseParser(new QESXMLResponseParser());
        return builder.build();
    }

    @Bean("solr-v1")
    public SolrTemplate solrTemplate() {
        SolrTemplate template = new SolrTemplate(solrV1Client());
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
    public SolrV1QueryBuilder builderV1Query() {
        return new SolrV1QueryBuilder();
    }
}


