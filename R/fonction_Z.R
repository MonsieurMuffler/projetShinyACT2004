Z <- function(delta, t = seq(0, 100, 0.1))
{
    ## t doit être positif
    if (any(t < 0))
        stop("t doit être positif")

    ## Valeur actualisée
    exp(-delta * t)
}

devtools::document()
roxygen2::roxygenize()
