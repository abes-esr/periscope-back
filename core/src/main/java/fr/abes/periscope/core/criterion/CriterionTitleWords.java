package fr.abes.periscope.core.criterion;

import fr.abes.periscope.core.exception.CriterionOperatorMismatchException;
import fr.abes.periscope.core.exception.IllegalCriterionException;
import fr.abes.periscope.core.exception.IllegalOperatorException;
import fr.abes.periscope.core.util.TYPE_NOTICE;
import lombok.Getter;

import java.util.ArrayList;
import java.util.List;

/**
 * Représente un critère de recherche par mots du titre
 */
@Getter
public class CriterionTitleWords extends Criterion {

    /** Liste des mots du titre à rechercher */
    private List<String> titleWords = new ArrayList<>();

    /** Liste des connecteurs logiques entre les mots du titre
     * Exemple :
     * titleWordsOperators[0] pour connecter titleWords[0]
     * titleWordsOperators[1] pour connecter titleWords[0] et titleWords[1] */
    private List<String> titleWordOperators = new ArrayList<>();

    /**
     * Instancie un critère de recherche par mots du titre avec un autre bloc
     * @param blocOperator Connecteur logique du bloc
     * @param candidatesTitleWords Liste des mots du titre à rechercher
     * @param candidatesOperator Liste des connecteurs logiques entre les mots du tires
     * @exception CriterionOperatorMismatchException Si le nombre de critères et le nombre d'opérateurs ne sont pas cohérent
     * @exception IllegalOperatorException Si la liste de connecteurs contient des connecteurs inexistant ou interdit
     * @exception IllegalCriterionException Si la liste des critères est vide
     */
    public CriterionTitleWords(String blocOperator, List<String> candidatesTitleWords, List<String> candidatesOperator) {
        super(blocOperator, TYPE_NOTICE.BIBLIO);

        if (candidatesTitleWords.isEmpty()) {
            throw new IllegalCriterionException("Criteria list is empty");
        }

        if (candidatesOperator.size() != candidatesTitleWords.size()) {
            throw new CriterionOperatorMismatchException("Criteria list size mismatch the operators list size");
        }

        boolean onlyAcceptedOperator = candidatesOperator.stream().allMatch(operator -> (
                LogicalOperator.AND.equals(operator) ||
                        LogicalOperator.OR.equals(operator) ||
                        LogicalOperator.EXCEPT.equals(operator)));

        if (!onlyAcceptedOperator) {
            throw new IllegalOperatorException("Operators contains illegal values. Accepted value : "+LogicalOperator.AND+"/"+LogicalOperator.OR+"/"+LogicalOperator.EXCEPT);
        }

        this.titleWords = candidatesTitleWords;
        this.titleWordOperators = candidatesOperator;
    }

    /**
     * Instancie un critère de recherche par mots du titre (1er bloc)
     * @param candidatesTitleWords Liste des mots du titre à rechercher
     * @param candidatesOperator Liste des connecteurs logiques entre les mots du titre
     * @exception CriterionOperatorMismatchException Si le nombre de critères et le nombre d'opérateurs ne sont pas cohérent
     * @exception IllegalOperatorException Si la liste de connecteurs contient des connecteurs inexistant ou interdit
     * @exception IllegalCriterionException Si la liste des critères est vide
     */
    public CriterionTitleWords(List<String> candidatesTitleWords, List<String> candidatesOperator) {
        super(TYPE_NOTICE.BIBLIO);

        if (candidatesTitleWords.isEmpty()) {
            throw new IllegalCriterionException("Criteria list is empty");
        }

        if (candidatesOperator.size() != candidatesTitleWords.size()) {
            throw new CriterionOperatorMismatchException("Criteria list size mismatch the operators list size");
        }

        boolean onlyAcceptedOperator = candidatesOperator.stream().allMatch(operator -> (
                LogicalOperator.AND.equals(operator) ||
                        LogicalOperator.OR.equals(operator) ||
                        LogicalOperator.EXCEPT.equals(operator)));

        if (!onlyAcceptedOperator) {
            throw new IllegalOperatorException("Operators contains illegal values. Accepted value : "+LogicalOperator.AND+"/"+LogicalOperator.OR+"/"+LogicalOperator.EXCEPT);
        }

        this.titleWords = candidatesTitleWords;
        this.titleWordOperators = candidatesOperator;
    }
}
