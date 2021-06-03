package fr.abes.periscope.core.entity.visualisation;

import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.*;
import java.util.stream.Collectors;

@Data
public class Holding {
    private final String COLOR_TXT_LACUNE = "";

    private String id;
    private String rcr;
    private String textEtatCollection;
    private List<Sequence> sequences;
    private Lacune lacune;
    private List<String> erreurs;

    public Holding(String id) {
        this.id = id;
        this.sequences = new ArrayList<>();
    }

    public Holding() {
        this.sequences = new ArrayList<>();
    }

    public void addSequence(Sequence sequence) {
        this.sequences.add(sequence);
    }

    public void addErreur(String message) {
        erreurs.add(message);
    }

    /**
     * Méthode permettant de fournir les séquences des états de collection d'une notice ordonnées et en intercalant les lacunes
     * @Return : liste de séquences ordonnées
     *
     */
    public List<Sequence> getSequencesOrdonnees() {
        List<Sequence> newListeSequence = new LinkedList<>();
        //tri de la liste des séquences par année de début croissante
        Collections.sort(this.getSequences(), Comparator.comparing(s -> s.getBlocDebut().getDate().getTime()));
        //filtrage des années en doublon dans les listeBlocLacunes, et tri par année de lacune croissante
        Set<Integer> yearSet = new HashSet<>();
        List<Bloc> listeBlocLacunes = new LinkedList<>();
        if (this.getLacune() != null) {
            listeBlocLacunes = this.getLacune().getBlocs().stream().filter(l -> yearSet.add(l.getDate().get(Calendar.YEAR))).collect(Collectors.toList());
        }
        Collections.sort(listeBlocLacunes, Comparator.comparing(l -> l.getDate().getTime()));
        this.getSequences().forEach(sequence -> {
            String commentaireLacune = this.getLacune().getCommentaire();
            boolean lacuneTrouve = false;

            //on vérifie la présence d'une lacune entre les dates de début / fin de la séquence
            for (Bloc blocLacune : this.getLacune().getBlocs()) {
                if (sequence.getBlocFin() != null) {
                    //cas ou la séquence dispose d'une date de début et de fin
                    if (blocLacune.getDate().getTime().after(sequence.getBlocDebut().getDate().getTime())
                            && blocLacune.getDate().getTime().before(sequence.getBlocFin().getDate().getTime())) {
                        newListeSequence.addAll(cutSequenceInThree(sequence, commentaireLacune, blocLacune, true));
                        lacuneTrouve = true;
                    }
                } else {
                    if (blocLacune.getDate().getTime().after(sequence.getBlocDebut().getDate().getTime())) {
                        //cas ou la séquence n'a pas de date de fin connue (toujours en cours)
                        newListeSequence.addAll(cutSequenceInThree(sequence, commentaireLacune, blocLacune, false));
                        lacuneTrouve = true;
                    }
                }
            }
            if (!lacuneTrouve) {
                //si on n'a pas trouvé de listeBlocLacunes dans la séquence, on la recopie telle quelle
                newListeSequence.add(sequence);
            }
        });
        return newListeSequence;
    }

    /**
     * Méthode de découpage d'une séquence en 3 partie : début de la séquence, lacune, fin de la séquence
     *
     * @param sequence          séquence à découper
     * @param commentaireLacune commentaire des lacunes
     * @param blocLacune        année de lacune à intercaller
     * @param withEndBloc       true si la sequence a une date de fin connue, false sinon
     * @return liste des 3 sequences construites
     */
    private List<Sequence> cutSequenceInThree(Sequence sequence, String commentaireLacune, Bloc blocLacune, boolean withEndBloc) {
        List<Sequence> newListeSequence = new LinkedList<>();
        //lacune présente dans la séquence, on découpe la séquence en 3 partie
        //début de la séquence initiale + année de lacune + fin de la séquence initiale
        BlocDebut blocDebutStartSequence = new BlocDebut(sequence.getBlocDebut().getDate(), sequence.getBlocDebut().getVolume(), sequence.getBlocDebut().getNumero());
        BlocFin blocFinStartSequence = new BlocFin(new GregorianCalendar(blocLacune.getDate().get(Calendar.YEAR) - 1, Calendar.DECEMBER, 31), "", "");
        Sequence sequenceDebut = new Sequence(blocDebutStartSequence, blocFinStartSequence, sequence.getMentionDeLacune(), sequence.getTexteEtatCollectionZone(), sequence.getNote(), EnumTypes.CONTINUE);

        newListeSequence.add(sequenceDebut);

        BlocDebut blocDebutLacunaire = new BlocDebut(new GregorianCalendar(blocLacune.getDate().get(Calendar.YEAR), Calendar.JANUARY, 1), blocLacune.getVolume(), blocLacune.getNumero());
        BlocFin blocFinLacunaire = new BlocFin(new GregorianCalendar(blocLacune.getDate().get(Calendar.YEAR), Calendar.DECEMBER, 31), "", "");
        Sequence sequenceLacunaire = new Sequence(blocDebutLacunaire, blocFinLacunaire, "", "", commentaireLacune, EnumTypes.LACUNE);

        newListeSequence.add(sequenceLacunaire);

        BlocDebut blocDebutEndSequence = new BlocDebut(new GregorianCalendar(blocLacune.getDate().get(Calendar.YEAR) + 1, Calendar.JANUARY, 1), "", "");
        Sequence sequenceFin;
        if (withEndBloc) {
            BlocFin blocFinEndSequence = new BlocFin(sequence.getBlocFin().getDate(), sequence.getBlocFin().getVolume(), sequence.getBlocFin().getNumero());
            sequenceFin = new Sequence(blocDebutEndSequence, blocFinEndSequence, "", "", "", EnumTypes.CONTINUE);
        } else {
            sequenceFin = new Sequence(blocDebutEndSequence, null, "", "", "", EnumTypes.CONTINUE);
        }

        newListeSequence.add(sequenceFin);
        return newListeSequence;
    }
}
