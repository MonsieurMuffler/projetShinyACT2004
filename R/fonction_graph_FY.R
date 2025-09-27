#' @title Graphique de la fonction de répartition de Y
#'
#' @description
#' Cette fonction permet de produire un graphique (ggplot) présentant la
#' fonction de répartition de Y selon le type de rente choisi. Il s'agit, en
#' quelque sorte, d'une fonction « wrapper ».
#'
#' @param ... arguments supplémentaires fournis à la fonction produisant le
#' graphique (fonction déterminée selon l'argument « type »).
#' @param type chaîne de caractères représentant le type de la rente. La valeur
#' par défaut est "via". Les choix sont les suivants :
#' - "via" : rente viagère
#' - "temp" : rente temporaire
#' - "diff" : rente différée
#' - "gar" : rente garantie
#'
#' @return graphique.
#'
#' @details
#' Pour plus de détails sur la création des graphiques, consultez les fonctions
#' associées à chaque type de rente.
#'
#' @import ggplot2
#'
#' @export
graph_FY <- function(..., type = c("via", "temp", "diff", "gar"))
{
    ## Validation de l'argument « type » et création du graphique de base
    graph <- switch(match.arg(type),
                    "via" = graph_FY_via(...),
                    "temp" = graph_FY_temp(...),
                    "diff" = graph_FY_diff(...),
                    "gar" = graph_FY_gar(...))

    ## Ajustements apportés au graphique de base
    graph +
        # Titre des axes
        labs(x = "x",
             y = TeX("$F_Y(x)$")) +
        # Format de l'axe des ordonnées
        scale_y_continuous(breaks = seq(0, 1, by = 0.1),
                           limits = c(0, 1),
                           labels = label_number(decimal.mark = ",")) +
        # Format de l'axe des abscisses
        scale_x_continuous(labels = label_number(decimal.mark = ",")) +
        # Arrière-plan blanc
        theme_bw()
}