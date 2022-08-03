package fr.abes.periscope.core.util;

import fr.abes.periscope.core.entity.xml.NoticesBibio;
import org.springframework.jdbc.core.RowMapper;

import java.sql.ResultSet;
import java.sql.SQLException;

public class NoticesBibioMapper implements RowMapper<NoticesBibio> {
    @Override
    public NoticesBibio mapRow(ResultSet resultSet, int i) throws SQLException {
        NoticesBibio notice = new NoticesBibio();
        notice.setId(resultSet.getInt("id"));
        notice.setDataXml(resultSet.getClob("data_xml"));
        return notice;
    }
}
