nqx <- function(methode = c("demoivre", "expo"), ...)
{
    switch(match.arg(methode),
           "demoivre" = nqx_demoivre(...),
           "expo" = nqx_expo(...))
}