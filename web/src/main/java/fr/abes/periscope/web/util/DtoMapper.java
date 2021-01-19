package fr.abes.periscope.web.util;

import fr.abes.periscope.core.util.TrackExecutionTime;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.stream.Collectors;

@Service
public class DtoMapper {

    @Autowired
    private ModelMapper modelMapper;

    @Bean
    public ModelMapper modelMapper() {
        return new ModelMapper();
    }

    @TrackExecutionTime
    public <S, T> List<T> mapList(List<S> source, Class<T> targetClass) {
        return source
                .stream()
                .map(element -> modelMapper.map(element, targetClass))
                .collect(Collectors.toList());
    }

    public <S, T> T map(S source, Class<T> targetClass) {
        return modelMapper.map(source, targetClass);
    }
}
