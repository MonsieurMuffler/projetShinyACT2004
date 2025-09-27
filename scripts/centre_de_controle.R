library(ggplot2)
library(latex2exp)
library(reshape2)
library(magrittr)
library(roxygen2)
library(devtools)
library(scales)
library(projetShinyACT2004)

usethis::use_description()
usethis::use_namespace()
devtools::document()

install.packages("renv")
detach("package:projetShinyACT2004", unload = TRUE)
install_github("MonsieurMuffler/projetShinyACT2004")
detach("package:projetShinyACT2004", unload = TRUE)
install.packages('digest', repos='http://cran.us.r-project.org')
Y

remove.packages("digest")
install.packages("digest")
