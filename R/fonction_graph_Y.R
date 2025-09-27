#' @title Graphique de Y en fonction de T
#'
#' @description
#' Cette fonction permet de produire un graphique (ggplot) de Y en fonction de T
#' selon le type de rente choisi. Il s'agit, en quelque sorte, d'une fonction
#' « wrapper ».
#'
#' @param ... arguments supplémentaires fournis à la fonction produisant le
#' graphique (fonction déterminée selon l'argument « type »).
#' @param x nombre réel positif représentant l'âge initial de la personne
#' assurée.
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
graph_Y <- function(..., x, type = c("via", "temp", "diff", "gar"))
{
    ## Validation de l'argument « type » et création du graphique de base
    graph <- switch(match.arg(type),
                    "via" = graph_Y_via(...),
                    "temp" = graph_Y_temp(...),
                    "diff" = graph_Y_diff(...),
                    "gar" = graph_Y_gar(...))

    ## Ajustements apportés au graphique de base
    graph +
        # Titre des axes
        labs(x = TeX(paste0("$T_{", x, "}$")),
             y = "Y") +
        # Format de l'axe des ordonnées
        scale_y_continuous(labels = label_number(decimal.mark = ","),
                           limits = c(0, max(list(...)$Y))) +
        # Format de l'axe des abscisses
        scale_x_continuous(labels = label_number(decimal.mark = ",")) +
        # Arrière-plan blanc
        theme_bw()
}