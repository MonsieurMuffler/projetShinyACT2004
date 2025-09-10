#' @export
Z <- function(delta, t = seq(0, 100, 0.1))
{
    ## t doit être positif
    if (any(t < 0))
        stop("t doit être positif")

    ## Valeur actualisée
    exp(-delta * t)
}

devtools::descrip
roxygen2::roxygenize()
usethis::use_description()
