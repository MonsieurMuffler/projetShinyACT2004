#' @title Graphique de la fonction de répartition de Y (rente viagère)
#'
#' @description
#' Cette fonction produit un graphique (ggplot) de la fonction de répartition
#' pour la variable aléatoire Y dans le cas d'une rente viagère.
#'
#' @param Y vecteur de nombres réels représentant les valeurs actualisées.
#' @param t vecteur de nombres réels représentant les durées utilisées pour
#' calculer les valeurs actualisées et les probabilités de décès.
#' @param tqx vecteur de nombres réels représentant les probabilités de décès.
#' @param n argument fantôme (voir les détails).
#' @param couleur chaîne de caractères définissant la couleur des courbes. La
#' valeur par défaut est "black".
#'
#' @return graphique.
#'
#' @details
#' Les paramètres « Y », « t » et « tqx » doivent être de la même longueur.
#' Normalement, les trois arguments doivent provenir du même vecteur « t ».
#' L'argument « n » n'est pas utilisé; sa présence simplifie le code de
#' l'application Shiny.
#'
#' @import ggplot2
#'
#' @export
graph_FY_via <- function(Y, t, tqx, n, couleur = "black")
{
    ## Graphique
    ggplot() +
        # Courbe principale
        geom_line(aes(x = Y, y = tqx),
                  linewidth = 1,
                  col = couleur) +
        # Segment à F(y) = 0
        geom_segment(aes(x = -0.5, xend = 0, y = 0, yend = 0),
                     linewidth = 1,
                     col = couleur) +
        # Segment à F(y) = 1
        geom_segment(aes(x = max(Y), xend = max(Y) * 1.05, y = 1, yend = 1),
                     linewidth = 1,
                     col = couleur) +
        # Titre
        labs(title = "Fonction de répartition (rente viagère)")
}