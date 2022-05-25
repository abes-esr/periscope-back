package fr.abes.periscope.core.util.binaryTree;

import fr.abes.periscope.core.entity.visualisation.Sequence;
import lombok.NoArgsConstructor;

@NoArgsConstructor
public class Tree {
    public Node root;

    public Tree(Node n) {
        root = n;
    }

    /**
     * Permet d'ajouter une séquence à la racine de l'arbre
     * @param sequence séquence à ajouter
     */
    public void add(Sequence sequence) {
        root = add(root, sequence);
    }

    /**
     * Permet d'ajouter une séquence dans l'arbre
     * @param node le noeur de l'arbre à partir duquel on va insérer
     * @param sequence la séquence à ajouter
     * @return
     */
    public Node add(Node node, Sequence sequence) {
        if (node == null)
            //on a trouvé le node, on ajoute une feuille à l'arbre
            return new Node(sequence, null, null);
        //si la date de début du node est avant la date de début de la séquence on descend d'un cran à droite
        if (node.getElement().getStartDate().compareTo(sequence.getStartDate()) <= 0)
            node.setDroit(add(node.getDroit(), sequence));
        //si la date de début du node est après la date de début de la séquence, on descend d'un cran à gauche
        else if (node.getElement().getStartDate().compareTo(sequence.getStartDate()) > 0)
            node.setGauche(add(node.getGauche(), sequence));
        //on retourne le noeud courant sinon
        return node;
    }

    /**
     * Permet de chercher récursivement une séquence dans un sous arbre
     * @param node le sous arbre dans lequel on cherche
     * @param sequence la séquence à chercher
     * @return on sort quand on est en bout d'arbre
     */
    public Node search(Node node, Sequence sequence) {
        if (node == null) return null;
        //Si la date de début du node courant est avant la date de début de la séquence et la date de fin du node courant est après la date de début de la séquence
        if (node.getElement().getStartDate() <= sequence.getStartDate() && node.getElement().getEndDate() >= sequence.getStartDate()) return node;
        //Si la date de début du node courant est après la date de début de la séquence on cherche dans le sous arbre de gauche
        if (node.getElement().getStartDate() > sequence.getStartDate()) return search(node.getGauche(), sequence);
        //sinon on cherche dans le sous arbre de droite
        return search(node.getDroit(), sequence);
    }
}
