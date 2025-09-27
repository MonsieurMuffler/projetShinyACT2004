#' @title Graphique de Y en fonction de T (rente garantie)
#'
#' @description
#' Cette fonction produit un graphique (ggplot) de Y en fonction de T dans le
#' cas d'une rente garantie.
#'
#' @param Y vecteur de nombres réels représentant les valeurs actualisées.
#' @param t vecteur de nombres réels représentant les durées utilisées pour
#' calculer les valeurs actualisées et les probabilités de décès.
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
graph_Y_gar <- function(Y, t, n, couleur = "orange")
{
    ## Graphique
    ggplot() +
        # Segment pour les paiement garantis
        geom_segment(aes(x = 0, xend = n, y = Y[t == n], yend = Y[t == n]),
                     linewidth = 1,
                     col = couleur) +
        # Courbe principale
        geom_line(aes(x = t[t >= n], y = Y[t >= n]),
                  linewidth = 1,
                  col = couleur) +
        # Titre
        labs(title = "Valeur actualisée (rente garantie)")
}