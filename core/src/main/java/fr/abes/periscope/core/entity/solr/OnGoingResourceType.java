package fr.abes.periscope.core.entity.solr;

/**
 * Représente la correspondance entre les caractères UNIMARC et
 * la dénomination littérale des types de ressources continues
 *
 * a 	Périodique
 * b 	Collection de monographies
 * c 	Journal
 * d 	Répertoire/Annuaire
 * e 	Publication à feuillets mobiles
 * f 	Base de données à mise à jour
 * g 	Site web à mise à jour
 * h 	Blog
 * i 	Dépôt d'archives numériques
 * j 	Revue
 * m 	Magazine
 * n 	Bulletin
 * z 	Autres
 *
 */
public abstract class OnGoingResourceType {
    public static final String A = "Périodique";
    public static final String B = "Collection de monographies";
    public static final String C = "Journal";
    public static final String D = "Répertoire/Annuaire";
    public static final String E = "Publication à feuillets mobiles";
    public static final String F = "Base de données à mise à jour";
    public static final String G = "Site web à mise à jour";
    public static final String H = "Blog";
    public static final String I = "Dépôt d'archives numériques";
    public static final String J = "Revue";
    public static final String M = "Magazine";
    public static final String N = "Bulletin";
    public static final String Z = "Autre";
    public static final String X = "Non renseigné";
}
