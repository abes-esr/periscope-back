package fr.abes.periscope.core.criterion;

import fr.abes.periscope.core.exception.CriterionOperatorMismatchException;
import fr.abes.periscope.core.exception.IllegalCriterionException;
import fr.abes.periscope.core.exception.IllegalOperatorException;
import lombok.Getter;

import java.util.ArrayList;
import java.util.List;

/**
 * Représente un critère de recherche par éditeur
 */
@Getter
public class CriterionEditor extends Criterion {

    /** Liste des éditeurs à rechercher */
    private List<String> editors = new ArrayList<>();

    /** Liste des connecteurs logiques entre les éditeurs
     * Exemple :
     * editorsOperator[0] pour connecter editors[0]
     * editorsOperator[1] pour connecter editors[0] et editors[1] */
    private List<String> editorOperators = new ArrayList<>();

    /**
     * Instancie un critère de recherche par éditeur à connecter avec un autre bloc
     * @param blocOperator Connecteur logique du bloc
     * @param candidatesEditor Liste des éditeurs à rechercher
     * @param candidatesOperator Liste des connecteurs logiques entre les éditeurs
     * @exception CriterionOperatorMismatchException Si le nombre de critères et le nombre d'opérateurs ne sont pas cohérent
     * @exception IllegalOperatorException Si la liste de connecteurs contient des connecteurs inexistant ou interdit
     * @exception IllegalCriterionException Si la liste des critères est vide
     */
    public CriterionEditor(String blocOperator, List<String> candidatesEditor, List<String> candidatesOperator) {
        super(blocOperator);

        if (candidatesEditor.isEmpty()) {
            throw new IllegalCriterionException("Criteria list is empty");
        }

        if (candidatesOperator.size() != candidatesEditor.size()) {
            throw new CriterionOperatorMismatchException("Criteria list size mismatch the operators list size");
        }

        boolean onlyAcceptedOperator = candidatesOperator.stream().allMatch(operator -> (
                LogicalOperator.AND.equals(operator) ||
                        LogicalOperator.OR.equals(operator) ||
                        LogicalOperator.EXCEPT.equals(operator)));

        if (!onlyAcceptedOperator) {
            throw new IllegalOperatorException("Operators contains illegal values. Accepted value : "+LogicalOperator.AND+"/"+LogicalOperator.OR+"/"+LogicalOperator.EXCEPT);
        }

        this.editors = candidatesEditor;
        this.editorOperators = candidatesOperator;
    }

    /**
     * Instancie un critère de recherche par éditeurs (1er bloc)
     * Le connecteur logique du bloc par défaut est ET
     * @param candidatesEditor Liste des éditeurs à rechercher
     * @param candidatesOperator Liste des connecteurs logiques entre les éditeurs
     * @exception CriterionOperatorMismatchException Si le nombre de critères et le nombre d'opérateurs ne sont pas cohérent
     * @exception IllegalOperatorException Si la liste de connecteurs contient des connecteurs inexistant ou interdit
     * @exception IllegalCriterionException Si la liste des critères est vide
     */
    public CriterionEditor(List<String> candidatesEditor, List<String> candidatesOperator) {
        super();

        if (candidatesEditor.isEmpty()) {
            throw new IllegalCriterionException("Criteria list is empty");
        }

        if (candidatesOperator.size() != candidatesEditor.size()) {
            throw new CriterionOperatorMismatchException("Criteria list size mismatch the operators list size");
        }

        boolean onlyAcceptedOperator = candidatesOperator.stream().allMatch(operator -> (
                LogicalOperator.AND.equals(operator) ||
                        LogicalOperator.OR.equals(operator) ||
                        LogicalOperator.EXCEPT.equals(operator)));

        if (!onlyAcceptedOperator) {
            throw new IllegalOperatorException("Operators contains illegal values. Accepted value : "+LogicalOperator.AND+"/"+LogicalOperator.OR+"/"+LogicalOperator.EXCEPT);
        }

        this.editors = candidatesEditor;
        this.editorOperators = candidatesOperator;
    }
}
