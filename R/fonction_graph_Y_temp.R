#' @title Graphique de Y en fonction de T (rente temporaire)
#'
#' @description
#' Cette fonction produit un graphique (ggplot) de Y en fonction de T dans le
#' cas d'une rente temporaire.
#'
#' @param Y vecteur de nombres réels représentant les valeurs actualisées.
#' @param t vecteur de nombres réels représentant les durées utilisées pour
#' calculer les valeurs actualisées et les probabilités de décès.
#' @param n nombre réel représentant la durée de la rente (période temporaire).
#' @param couleur chaîne de caractères définissant la couleur des courbes. La
#' valeur par défaut est "blue".
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
graph_Y_temp <- function(Y, t, n, couleur = "blue")
{
    ## Graphique
    ggplot() +
        # Courbe principale
        geom_line(aes(x = t[t <= n], y = Y[t <= n]),
                  linewidth = 1,
                  col = couleur) +
        # Segment pour t > n
        geom_segment(aes(x = n, xend = t[length(t)],
                         y = Y[t == n], yend = Y[t == n]),
                     linewidth = 1,
                     col = couleur) +
        # Titre
        labs(title = "Valeur actualisée (rente temporaire)")
}
