#' @title Fonction pour calculer la valeur actualisée d'une rente
#'
#' @description
#' Cette fonction permet de calculer la valeur actualisée d'une rente octroyant
#' 1$ de manière continue en fonction de la force d'intérêt et de la durée de
#' vie.
#'
#' @param delta nombre réel positif représentant la force d'intérêt.
#' @param t nombre réel positif représentant la durée de vie (ou, de manière
#' équivalente, le temps avant le décès).
#'
#' @return nombre réel représentant la valeur actualisée.
#'
#' @details
#' La fonction pourrait être complexifiée en permettant l'utilisation d'une
#' force d'intérêt qui varie dans le temps.
#'
#' @export
Y <- function(delta, t)
{
    ## Arguments positifs
    if (any(t < 0) || any(delta < 0))
        stop("Les arguments doivent être positifs")
    # Note : any() si on veut utiliser plusieurs valeurs en même temps.

    ## Valeur actualisée
    (1 - exp(-delta * t))/delta
}
