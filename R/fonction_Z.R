#' @title Fonction pour calculer la valeur actualisée d'une prestation
#'
#' @description
#' Cette fonction permet de calculer la valeur actualisée d'une prestation de 1$
#' en fonction de la force d'intérêt et de la durée de vie.
#'
#' @param delta nombre réel positif représentant la force d'intérêt.
#' @param t nombre réel positif représentant la durée de vie (ou, de manière
#' équivalente, le temps avant le décès).
#'
#' @return nombre réel entre 0 et 1 représentant la valeur actualisée.
#'
#' @details
#' La fonction pourrait être complexifiée en permettant l'utilisation d'une
#' force d'intérêt qui varie dans le temps.
#'
#' @export
Z <- function(delta, t)
{
    ## Arguments positifs
    if (any(t < 0) || any(delta < 0))
        stop("Les arguments doivent être positifs")
    # Note : any() si on veut utiliser plusieurs valeurs en même temps.

    ## Valeur actualisée
    exp(-delta * t)
}