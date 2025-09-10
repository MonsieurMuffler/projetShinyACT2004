#' @title Fonction de répartition de la durée de vie complète (exponentielle)
#'
#' @description
#' Cette fonction permet de calculer la probabilité de décéder dans un
#' intervalle de temps donné selon la loi exponentielle.
#'
#' @param t nombre réel positif représentant la durée de vie maximale (ou, de
#' manière équivalente, l'intervalle de temps à l'intérieur duquel la personne
#' va décéder). t peut aussi être un vecteur contenant plusieurs nombres.
#' @param mu nombre réel positif représentant la force de mortalité.
#'
#' @return nombre réel entre 0 et 1 représentant la probabilité que la personne
#' décède entre l'âge x et l'âge x + t. Si plusieurs t sont utilisés, la
#' fonction retourne un vecteur.
#'
#' @details
#' La loi exponentielle est utilisée; puisque cette loi est sans mémoire, l'âge
#' initial de la personne n'a pas d'importance.
#'
#' @export
tqx_expo <- function(t, mu)
{
    ## Arguments positifs
    if (mu < 0 || any(t < 0))
        stop("Les arguments doivent être positifs")

    ## Probabilité
    1 - exp(-mu * t)
}