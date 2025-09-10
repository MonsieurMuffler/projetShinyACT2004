#' @title Fonction de répartition de la durée de vie complète (De Moivre)
#'
#' @description
#' Cette fonction permet de calculer la probabilité de décéder dans un
#' intervalle de temps donné selon la loi de mortalité de De Moivre.
#'
#' @param x nombre réel positif représentant l'âge de la personne.
#' @param t nombre réel positif représentant la durée de vie maximale (ou, de
#' manière équivalente, l'intervalle de temps à l'intérieur duquel la personne
#' va décéder). t peut aussi être un vecteur contenant plusieurs nombres.
#' @param omega nombre réel positif représentant l'âge ultime.
#'
#' @return nombre réel entre 0 et 1 représentant la probabilité que la personne
#' décède entre l'âge x et l'âge x + t. Si plusieurs t sont utilisés, la
#' fonction retourne un vecteur.
#'
#' @details
#' La loi de mortalité de De Moivre est utilisée; cette loi suppose une
#' distribution uniforme des décès entre l'âge initial et l'âge ultime.
#'
#' @export
tqx_demoivre <- function(x, t, omega)
{
    ## Arguments positifs
    if (x < 0 || omega < 0 || any(t < 0))
        stop("Les arguments doivent être positifs")

    ## Dénominateur
    denom <- omega - x

    ## Les arguments doivent mener à une probabilité
    if (denom <= 0 || any(t > denom))
        stop(paste0("Les arguments ne peuvent pas être utilisés avec la loi de",
                    " mortalité de De Moivre"))

    ## Probabilité
    t/(omega - x)
}