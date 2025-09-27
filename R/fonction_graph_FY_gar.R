#' @title Graphique de la fonction de répartition de Y (rente garantie)
#'
#' @description
#' Cette fonction produit un graphique (ggplot) de la fonction de répartition
#' pour la variable aléatoire Y dans le cas d'une rente garantie.
#'
#' @param Y vecteur de nombres réels représentant les valeurs actualisées.
#' @param t vecteur de nombres réels représentant les durées utilisées pour
#' calculer les valeurs actualisées et les probabilités de décès.
#' @param tqx vecteur de nombres réels représentant les probabilités de décès.
#' @param n nombre réel représentant la durée des paiements garantis.
#' @param couleur chaîne de caractères définissant la couleur des courbes. La
#' valeur par défaut est "orange".
#'
#' @return graphique.
#'
#' @details
#' Les paramètres « Y », « t » et « tqx » doivent être de la même longueur.
#' Normalement, les trois arguments doivent provenir du même vecteur « t ».
#'
#' @import ggplot2
#'
#' @export
graph_FY_gar <- function(Y, t, tqx, n, couleur = "orange")
{
    ## Graphique
    ggplot() +
        # Courbe principale
        geom_line(aes(x = Y[t >= n], y = tqx[t >= n]),
                  linewidth = 1,
                  col = couleur) +
        # Segment à F(y) = 0
        geom_segment(aes(x = -0.5, xend = Y[t == n], y = 0, yend = 0),
                     linewidth = 1,
                     col = couleur) +
        # Segment à F(y) = 1
        geom_segment(aes(x = max(Y), xend = max(Y) * 1.05, y = 1, yend = 1),
                     linewidth = 1,
                     col = couleur) +
        # Point ouvert
        geom_point(aes(x = Y[t == n], y = 0),
                   fill = "white",
                   col = couleur,
                   pch = 21,
                   size = 2) +
        # Point fermé
        geom_point(aes(x = Y[t == n], y = tqx[t == n]),
                   size = 2,
                   col = couleur) +
        # Titre
        labs(title = "Fonction de répartition (rente garantie)")
}