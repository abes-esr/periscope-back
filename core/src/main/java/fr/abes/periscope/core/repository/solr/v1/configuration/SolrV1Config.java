package fr.abes.periscope.core.repository.solr.v1.configuration;

import fr.abes.periscope.core.repository.solr.v1.SolrQueryBuilder;
import org.apache.solr.client.solrj.SolrClient;
import org.apache.solr.client.solrj.impl.HttpSolrClient;
import org.apache.solr.client.solrj.impl.XMLResponseParser;
import org.apache.solr.common.SolrException;
import org.apache.solr.common.params.ModifiableSolrParams;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.solr.core.SolrTemplate;

/**
 * Configuration du client SolR
 */
@Deprecated
@Configuration
public class SolrV1Config {

    @Value("${solr.v1.baseurl}")
    private String baseURL;

    @Bean
    public SolrClient solrClient() {

        if (baseURL.isEmpty()) {
            throw  new SolrException(SolrException.ErrorCode.SERVER_ERROR,"baseURL is empty");
        }

        ModifiableSolrParams params = new ModifiableSolrParams();
        params.add("solrService","Pcp");
        params.add("wt", "xml");
        params.add("version","2.2");
        params.add("indent", "on");
        params.add("omitHeader","false");

        HttpSolrClient.Builder builder = new HttpSolrClient.Builder()
                .withBaseSolrUrl(this.baseURL)
                .withInvariantParams(params)
                .withResponseParser(new QESXMLResponseParser());
        return builder.build();
    }

    @Bean
    @Qualifier("solrV1Template")
    public SolrTemplate solrV1Template() {
        SolrTemplate template = new SolrTemplate(solrClient());
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
    @Qualifier("SolrQueryV1Builder")
    public SolrQueryBuilder builderV1Query() {
        return new SolrQueryBuilder();
    }
}


