#' @title Fonction de répartition de la durée de vie complète
#'
#' @description
#' Cette fonction permet de calculer la probabilité de décéder dans un
#' intervalle de temps donné selon une loi de mortalité quelconque.
#'
#' @param methode chaîne de caractères définissant la méthode (la loi/la
#' fonction) à utiliser.
#' @param ... arguments supplémentaires fournis à la fonction utilisée pour
#' calculer la probabilité de décès.
#'
#' @return résultat dans un format spécifique à la fonction choisie à l'aide de
#' l'argument « methode ».
#'
#' @details
#' Les lois pouvant être utilisées actuellement sont la loi de De Moivre (voir
#' la fonction « tqx_demoivre ») et la loi exponentielle (voir la fonction
#' « tqx_expo »). Il est simple d'ajouter d'autres lois.
#'
#' @export
tqx <- function(methode = c("demoivre", "expo"), ...)
{
    switch(match.arg(methode),
           "demoivre" = tqx_demoivre(...),
           "expo" = tqx_expo(...))
}