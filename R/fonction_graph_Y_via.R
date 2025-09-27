#' @title Graphique de Y en fonction de T (rente viagère)
#'
#' @description
#' Cette fonction produit un graphique (ggplot) de Y en fonction de T dans le
#' cas d'une rente viagère.
#'
#' @param Y vecteur de nombres réels représentant les valeurs actualisées.
#' @param t vecteur de nombres réels représentant les durées utilisées pour
#' calculer les valeurs actualisées et les probabilités de décès.
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
graph_Y_via <- function(Y, t, n, couleur = "black")
{
    ## Graphique
    ggplot() +
        # Courbe principale
        geom_line(aes(x = t, y = Y),
                  linewidth = 1,
                  col = couleur) +
        # Titre
        labs(title = "Valeur actualisée (rente viagère)")
}
