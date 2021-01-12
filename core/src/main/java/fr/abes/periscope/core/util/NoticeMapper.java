package fr.abes.periscope.core.util;

import fr.abes.periscope.core.entity.Notice;
import fr.abes.periscope.core.entity.NoticeSolr;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.stereotype.Component;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

@Component
@Slf4j
public class NoticeMapper {

    @Autowired
    private ModelMapper modelMapper;

    @Bean
    public ModelMapper modelMapper() {
        return new ModelMapper();
    }

    public Date extractStartDate(String value) throws ParseException {
        //log.debug("SolR startdate : "+value.substring(0,8));
        return new SimpleDateFormat("yyyyMMdd").parse(value.substring(0,8));
    }

    public Date extractEndDate(String value) throws ParseException {
        //log.debug("SolR enddate : "+value.substring(9,17));
        return new SimpleDateFormat("yyyyMMdd").parse(value.substring(9,17));
    }

    @TrackExecutionTime
    public List<Notice> mapList(List<NoticeSolr> source) {
        return source
                .stream()
                .map(element -> map(element))
                .collect(Collectors.toList());
    }

    public Notice map(NoticeSolr source) {
        Notice notice = modelMapper.map(source, Notice.class);

        // Extraction de la date de début
        try {
            notice.setStartDate(extractStartDate(source.getProcessingGlobalData()));
        } catch (ParseException e) {
            //log.debug("SolR startdate : '"+source.getProcessingGlobalData().substring(0,8)+"'");
            //log.debug("Unable to parse start date :"+e.getLocalizedMessage());
            notice.setStartDate(null);
        }

        // Extraction de la date de fin
        try {
            notice.setEndDate(extractEndDate(source.getProcessingGlobalData()));
        } catch (ParseException e) {
            //log.debug("SolR enddate : '"+source.getProcessingGlobalData().substring(9,17)+"'");
            //log.debug("Unable to parse end date :"+e.getLocalizedMessage());
            notice.setEndDate(null);
        }

        return notice;
    }
}
