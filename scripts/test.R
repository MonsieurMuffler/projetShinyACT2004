




df <- function(delta, ages = seq(0, 100, 0.1), n, ...)
{
    ## Probabilité de décès
    nqx <- nqx(ages = ages, ...)
    npx <- 1 - nqx

    ## Toutes les valeurs actualisées
    z <- Z(delta, ages)

    ## Vecteur vide
    z_empty <- numeric(length(ages))

    ## Position avant/après la durée du contrat temporaire
    avant_n <- ages < n
    apres_n <- ages >= n
    a_n <- ages == n

    ## Assurance vie entière
    z_ent <- z
    Fz_ent <- npx

    ## Assurance vie temporaire
    z_temp <- z * avant_n
    Fz_temp <- Fz_ent
    Fz_temp[apres_n] <- max(Fz_temp[apres_n])

    ## Capital différé
    z_cap_diff <- z_empty
    z_cap_diff[apres_n] <- z[a_n]
    Fz_cap_diff <- z_empty
    Fz_cap_diff[avant_n] <- npx[a_n]
    Fz_cap_diff[apres_n] <- 1

    ## Assurance mixte
    z_mixte <- z_cap_diff
    z_mixte[avant_n] <- z[avant_n]
    Fz_mixte <- Fz_ent
    Fz_mixte[apres_n] <- 0

    ## Assurance différée
    z_diff <- z
    z_diff[avant_n] <- 0
    Fz_diff <- Fz_ent
    Fz_diff[avant_n] <- 1


    ## Tableau de données contenant toutes les informations
    df <- data.frame("age" = ages,
                     "nqx" = nqx,
                     "z_ent" = z_ent,
                     "z_temp" = z_temp,
                     "z_cap_diff" = z_cap_diff,
                     "z_mixte" = z_mixte,
                     "z_diff" = z_diff)

    ## Tableau de données fondu pour usage avec ggplot2
    dff <- melt(df,
                id.vars = c("age", "nqx"),
                variable.name = "type",
                value.name = "valeur")

    ## Tableau final
    df_final <- data.frame(dff, "Fz" = c(Fz_ent,
                                         Fz_temp,
                                         Fz_cap_diff,
                                         Fz_temp,
                                         Fz_mixte))

}

repart <- function(Fx, val)
{
    sapply(unique(val),
           function(x)
           {
               pos <- val == x
               if (length(pos) == 1)
                   return(Fx[pos])
               vec <- Fx[pos]
               vec[] <- max(Fx[pos])
               vec
           }) %>% unlist()
}

df_test <- df_demoivre

lapply(unique(df_test$type),
       function(x)
       {

           df_red <- df_test[df_test$type == x, ]
           #df_red <- df_test[df_test$type == "z_temp", ]
           df_red$FZ <- repart(df_red$FZ, df_red$val)
           df_red
       })


val <- c(0, 0, 0.3, 0.2, 0.1)
ff <- c(0.2, 0.3, 0.4, 0.7, 1)
aa <- c(1, 2,3,4,5)

or <- order(val, decreasing = TRUE)
val[or]
ff_t <- 1 - ff[or]
ff_t <- ff_t + (1 - ff_t[1])
ff_t2 <- ff_t - (ff_t > 1)

ages <- seq(0, 100, 0.1)
delta <- 0.1
loi <- "demoivre"
omega <- 80
mu <- 0.03
n <- 50

df_demoivre <- df(delta, ages, n, "demoivre", omega)
df_expo <- df(delta, ages, n, "expo", mu)


param <- paste0("Force d'intérêt : ", delta,
                "\nLoi : ", loi,
                "\nÂge ultime : ", omega)

options(OutDec = ",")
ggplot(df_demoivre[df_demoivre$type == "z_diff", ], aes(x = valeur, y = Fz)) +
    geom_line(aes(col = type), linewidth = 1) +
    labs(y = TeX("$F_Z(x)$"),
         x = TeX("x"),
         title = "Fonction de répartition de la variable aléatoire Z",
         subtitle = param) +
    theme_minimal()

ggplot(df_expo, aes(x = valeur, y = 1 - nqx)) +
    geom_line(aes(col = type), linewidth = 1) +
    labs(y = TeX("$F_Z(x)$"),
         x = TeX("x"),
         title = "Fonction de répartition de la variable aléatoire Z",
         subtitle = param) +
    theme_minimal()

ggplot(df_demoivre, aes(x = age, y = valeur)) +
    geom_line(aes(col = type, linetype = type), linewidth = 1) +
    labs(y = TeX("$Z$"),
         x = TeX("T"),
         title = "Valeur actualisée de la prestation de décès en fonction de l'âge atteint",
         subtitle = param) +
    theme_minimal()

ggplot(df_expo, aes(x = age, y = valeur)) +
    geom_line(aes(col = type, linetype = type), linewidth = 1) +
    labs(y = TeX("$Z$"),
         x = TeX("T"),
         title = "Valeur actualisée de la prestation de décès en fonction de l'âge atteint",
         subtitle = param) +
    theme_minimal()

plot(val_actu(0.1, ages), 1 - nqx_demoivre(80, ages), type = "l", lty = 1)
plot(val_actu(0.05, ages), 1 - nqx_expo(0.03, ages), type = "l", lty = 1)


prob(methode = "expo", 0.03)

prob()

1 + log(0.1)/(2*3)


exp((0.6162358 - 1) * 6)

val_actu(0.1)


val_actu(2)
.Primitive("sqrt")
sum(2)
UseMethod()

val.x <- function(s, ee)
{
    s + ee
}
val.y <- function(s)
{
    s*2
}
val <- function(s, ...)
{
    UseMethod("val")
}

ccc <- 2
class(ccc) <- c(class(ccc), "x")
val(ccc, 2)


print.x <- function(x, ...)
{
    class(x) <- NULL
    print.default(x)
}




# Sample data
set.seed(123)
x <- sample(c(1, 2, 3, 5, 8), size = 50, replace = TRUE)

# Create ECDF data manually (only horizontal segments)
ecdf_data <- ecdf(x)
x_vals <- sort(unique(x))
y_vals <- ecdf_data(x_vals)

df <- data.frame(
    x_start = head(x_vals, -1),
    x_end = tail(x_vals, -1),
    y = head(y_vals, -1)
)

# Plot only horizontal segments
ggplot(df) +
    geom_segment(aes(x = x_start, xend = x_end, y = y, yend = y)) +
    labs(title = "ECDF with Gaps (No Vertical Lines)",
         x = "x", y = "F(x)") +
    theme_minimal()