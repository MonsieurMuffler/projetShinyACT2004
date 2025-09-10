nqx_demoivre <- function(omega, ages = seq(0, 100, 0.1))
{
    pmax(pmin(ages/omega, 1), 0)
}