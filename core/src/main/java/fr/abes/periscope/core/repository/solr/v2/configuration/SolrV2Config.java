package fr.abes.periscope.core.repository.solr.v2.configuration;

import fr.abes.periscope.core.repository.solr.v1.SolrV1QueryBuilder;
import fr.abes.periscope.core.repository.solr.v2.SolrV2QueryBuilder;
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
public class SolrV2Config {

    @Bean
    public SolrClient solrV2Client() {
        ModifiableSolrParams params = new ModifiableSolrParams();
        params.add("wt", "xml");
        params.add("version","2.2");
        params.add("indent", "on");
        params.add("omitHeader","false");

        HttpSolrClient.Builder builder = new HttpSolrClient.Builder()
                .withBaseSolrUrl("http://smalt-dev.v212.abes.fr:8081/solr/")
                .withInvariantParams(params)
                .withResponseParser(new QESXMLResponseParser());
        return builder.build();
    }

    @Bean("solr-v2")
    public SolrTemplate solrTemplate() {
        SolrTemplate template = new SolrTemplate(solrV2Client());
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
    public SolrV2QueryBuilder builderV2Query() {
        return new SolrV2QueryBuilder();
    }
}


