package fr.abes.periscope;

import lombok.Setter;
import org.springframework.batch.item.database.support.OraclePagingQueryProvider;
import org.springframework.stereotype.Component;

@Setter
@Component
public class NoticeQueryProvider extends OraclePagingQueryProvider {

    @Override
    public String generateFirstPageQuery(int pageSize) {
        String query = createQuery();
        query += " order by id asc";
        query += " OFFSET 0 ROWS FETCH NEXT " + pageSize + " ROWS ONLY";
        return query;
    }

    @Override
    public String generateRemainingPagesQuery(int pageSize) {
        String query = createQuery();
        query += " order by id asc";
        query += " OFFSET 0 ROWS FETCH NEXT " + pageSize + " ROWS ONLY";
        return query;
    }

    @Override
    public String generateJumpToItemQuery(int index, int pageSize) {
        String query = createQuery();
        query += " OFFSET " + index + " ROWS FETCH NEXT " + pageSize + " ROWS ONLY";
        return query;
    }

    private String createQuery() {
        StringBuilder query = new StringBuilder("select ");
        query.append(this.getSelectClause());
        query.append(" from ");
        query.append(this.getFromClause());
        query.append(" where ");
        query.append(this.getWhereClause());
        return query.toString();
    }

}
