package fr.abes.periscope.core.entity.visualisation;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.Calendar;

@Getter
@Setter
@NoArgsConstructor
public class BlocDebut extends Bloc {

    public BlocDebut(Calendar date, String volume, String numero) {
        super(date, volume, numero);
    }
}
