nqx_expo <- function(mu, ages = seq(0, 100, 0.1))
{
    pmin(1 - exp(-mu * ages), 1)
}