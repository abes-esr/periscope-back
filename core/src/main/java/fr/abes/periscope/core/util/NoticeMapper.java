package fr.abes.periscope.core.util;

import fr.abes.periscope.core.entity.Notice;
import fr.abes.periscope.core.entity.NoticeSolr;
import fr.abes.periscope.core.entity.PublicationYear;
import fr.abes.periscope.core.exception.IllegalPublicationYearException;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.stereotype.Component;

import java.text.ParseException;
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

    public PublicationYear buildStartPublicationYear(String value) throws ParseException {
        //log.debug("SolR startdate : "+value.substring(9,13));
        String yearCode = value.substring(8,9);
        PublicationYear year = new PublicationYear();
        switch (yearCode) {
            case "b":
                String candidateYear = value.substring(9,13);
                if(candidateYear.charAt(2)==' ' && candidateYear.charAt(3)==' ') {
                    year.setYear(Integer.valueOf(candidateYear.substring(0,2)));
                    year.setConfidenceIndex(100);
                }
                else if(candidateYear.charAt(2)==' ') {
                   new IllegalPublicationYearException("Unable to decode year format like"+candidateYear);

                } else if(candidateYear.charAt(3)==' ') {
                    year.setYear(Integer.valueOf(candidateYear.substring(0,3)));
                    year.setConfidenceIndex(10);
                }else {
                    year.setYear(Integer.valueOf(candidateYear.substring(0,4)));
                    year.setConfidenceIndex(0);
                }
                return year;
            default:
                throw new IllegalPublicationYearException("Unable to decode year code "+yearCode);
        }

    }

    public PublicationYear buildEndPublicationYear(String value) throws ParseException {
        //log.debug("SolR enddate : "+value.substring(13,17));
        String yearCode = value.substring(8,9);
        PublicationYear year = new PublicationYear();

        switch (yearCode) {
            case "b":
                String candidateYear = value.substring(13,17);
                if(candidateYear.charAt(2)==' ' && candidateYear.charAt(3)==' ') {
                    year.setYear(Integer.valueOf(candidateYear.substring(0,2)));
                    year.setConfidenceIndex(100);
                }
                else if(candidateYear.charAt(2)==' ') {
                    new IllegalPublicationYearException("Unable to decode year format like"+candidateYear);

                } else if(candidateYear.charAt(3)==' ') {
                    year.setYear(Integer.valueOf(candidateYear.substring(0,3)));
                    year.setConfidenceIndex(10);
                }else {
                    year.setYear(Integer.valueOf(candidateYear.substring(0,4)));
                    year.setConfidenceIndex(0);
                }
                return year;
            default:
                throw new IllegalPublicationYearException("Unable to decode year code "+yearCode);
        }
    }

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

            PublicationYear year = buildStartPublicationYear(source.getProcessingGlobalData());
            notice.setStartYear(year);
        } catch (ParseException e) {
            //log.debug("SolR startdate : '"+source.getProcessingGlobalData().substring(0,8)+"'");
            //log.debug("Unable to parse start date :"+e.getLocalizedMessage());
            notice.setStartYear(null);
        }

        // Extraction de la date de fin
        try {
            PublicationYear year = buildEndPublicationYear(source.getProcessingGlobalData());
            notice.setEndYear(year);
        } catch (ParseException e) {
            //log.debug("SolR enddate : '"+source.getProcessingGlobalData().substring(9,17)+"'");
            //log.debug("Unable to parse end date :"+e.getLocalizedMessage());
            notice.setEndYear(null);
        }

        return notice;
    }
}
