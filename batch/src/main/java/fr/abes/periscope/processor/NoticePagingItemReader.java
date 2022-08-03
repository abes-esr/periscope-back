package fr.abes.periscope.processor;

import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.batch.item.ItemStreamException;
import org.springframework.batch.item.database.AbstractPagingItemReader;
import org.springframework.batch.item.database.Order;
import org.springframework.batch.item.database.PagingQueryProvider;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.util.Assert;
import org.springframework.util.ClassUtils;

import javax.sql.DataSource;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.*;
import java.util.concurrent.CopyOnWriteArrayList;

@Slf4j
public class NoticePagingItemReader<T> extends AbstractPagingItemReader<T> implements InitializingBean {
    private static final String START_AFTER_VALUE = "start.after";
    public static final int VALUE_NOT_SET = -1;
    private DataSource dataSource;
    private PagingQueryProvider queryProvider;
    private Map<String, Object> parameterValues;
    private NamedParameterJdbcTemplate namedParameterJdbcTemplate;
    @Setter
    private RowMapper<T> rowMapper;
    private String firstPageSql;
    private String remainingPagesSql;
    private Map<String, Object> startAfterValues;
    private Map<String, Object> previousStartAfterValues;
    private int fetchSize = -1;

    public NoticePagingItemReader() {
        this.setName(ClassUtils.getShortName(NoticePagingItemReader.class));
    }

    public void setDataSource(DataSource dataSource) {
        this.dataSource = dataSource;
    }

    public void setFetchSize(int fetchSize) {
        this.fetchSize = fetchSize;
    }

    public void setQueryProvider(PagingQueryProvider queryProvider) {
        this.queryProvider = queryProvider;
    }

    public void setRowMapper(RowMapper<T> rowMapper) {
        this.rowMapper = rowMapper;
    }

    public void setParameterValues(Map<String, Object> parameterValues) {
        this.parameterValues = parameterValues;
    }

    public void afterPropertiesSet() throws Exception {
        super.afterPropertiesSet();
        Assert.notNull(this.dataSource, "DataSource may not be null");
        JdbcTemplate jdbcTemplate = new JdbcTemplate(this.dataSource);
        if (this.fetchSize != -1) {
            jdbcTemplate.setFetchSize(this.fetchSize);
        }
        jdbcTemplate.setMaxRows(this.getPageSize());
        this.namedParameterJdbcTemplate = new NamedParameterJdbcTemplate(jdbcTemplate);
        Assert.notNull(this.queryProvider, "QueryProvider may not be null");
        this.queryProvider.init(this.dataSource);
        this.firstPageSql = this.queryProvider.generateFirstPageQuery(this.fetchSize);
        this.remainingPagesSql = this.queryProvider.generateRemainingPagesQuery(this.fetchSize);
    }

    protected void doReadPage() {
            long startTime = System.currentTimeMillis();
            if (this.results == null) {
                this.results = new CopyOnWriteArrayList();
            } else {
                this.results.clear();
            }

            PagingRowMapper rowCallback = new PagingRowMapper();
            List query;
            if (this.getPage() == 0) {
                log.debug("SQL used for reading first page: [" + this.firstPageSql + "]");

                if (this.parameterValues != null && this.parameterValues.size() > 0) {
                    query = this.namedParameterJdbcTemplate.query(this.firstPageSql, this.getParameterMap(this.parameterValues, (Map) null), rowCallback);
                } else {
                    query = this.getJdbcTemplate().query(this.firstPageSql, rowCallback);
                }
            } else {
                this.previousStartAfterValues = this.startAfterValues;
                Integer minValue = (Integer)this.parameterValues.get("minValue");
                minValue += this.fetchSize;
                this.parameterValues.put("minValue", minValue);
                String jumpToQuery = this.queryProvider.generateJumpToItemQuery(0, this.fetchSize);
                log.debug("SQL used for reading remaining pages: [" + jumpToQuery + "]");
                query = this.namedParameterJdbcTemplate.query(jumpToQuery, this.getParameterMap(this.parameterValues, this.startAfterValues), rowCallback);
            }

            this.results.addAll(query);
            long endTime = System.currentTimeMillis();
            log.debug("Traitement requête : " + (endTime - startTime) + "ms");
    }

    public void update(ExecutionContext executionContext) throws ItemStreamException {
        super.update(executionContext);
        if (this.isSaveState()) {
            if (this.isAtEndOfPage() && this.startAfterValues != null) {
                executionContext.put(this.getExecutionContextKey("start.after"), this.startAfterValues);
            } else if (this.previousStartAfterValues != null) {
                executionContext.put(this.getExecutionContextKey("start.after"), this.previousStartAfterValues);
            }
        }

    }

    private boolean isAtEndOfPage() {
        return this.getCurrentItemCount() % this.getPageSize() == 0;
    }

    public void open(ExecutionContext executionContext) {
        if (this.isSaveState()) {
            this.startAfterValues = (Map) executionContext.get(this.getExecutionContextKey("start.after"));
            if (this.startAfterValues == null) {
                this.startAfterValues = new LinkedHashMap();
            }
        }

        super.open(executionContext);
    }

    protected void doJumpToPage(int itemIndex) {
        if (this.startAfterValues == null && this.getPage() > 0) {
            String jumpToItemSql = this.queryProvider.generateJumpToItemQuery(itemIndex, this.getPageSize());
            log.debug("SQL used for jumping: [" + jumpToItemSql + "]");
            this.startAfterValues = this.namedParameterJdbcTemplate.queryForMap(jumpToItemSql, this.getParameterMap(this.parameterValues, (Map) null));
        }
    }

    private Map<String, Object> getParameterMap(Map<String, Object> values, Map<String, Object> sortKeyValues) {
        Map<String, Object> parameterMap = new LinkedHashMap();
        if (values != null) {
            parameterMap.putAll(values);
        }

        if (sortKeyValues != null && !sortKeyValues.isEmpty()) {
            Iterator iterator = sortKeyValues.entrySet().iterator();

            while (iterator.hasNext()) {
                Map.Entry<String, Object> sortKey = (Map.Entry) iterator.next();
                parameterMap.put("_" + (String) sortKey.getKey(), sortKey.getValue());
            }
        }
        log.debug("Using parameterMap:" + parameterMap);
        return parameterMap;
    }

    private JdbcTemplate getJdbcTemplate() {
        return (JdbcTemplate) this.namedParameterJdbcTemplate.getJdbcOperations();
    }

    private class PagingRowMapper implements RowMapper<T> {
        private PagingRowMapper() {
        }

        public T mapRow(ResultSet rs, int rowNum) throws SQLException {
            NoticePagingItemReader.this.startAfterValues = new LinkedHashMap();
            Iterator var3 = NoticePagingItemReader.this.queryProvider.getSortKeys().entrySet().iterator();

            while (var3.hasNext()) {
                Map.Entry<String, Order> sortKey = (Map.Entry) var3.next();
                NoticePagingItemReader.this.startAfterValues.put(sortKey.getKey(), rs.getObject((String) sortKey.getKey()));
            }

            return NoticePagingItemReader.this.rowMapper.mapRow(rs, rowNum);
        }
    }
}

