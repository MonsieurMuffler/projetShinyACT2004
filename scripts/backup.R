graph_Z_ent <- function(Z, t, n)
{
    ggplot(mapping = aes(x = t, y = Z)) +
        geom_line(linewidth = 1) +
        labs(title = "Vie entière")
}
graph_Z_ent(the_Z, the_t)


graph_Z_temp <- function(Z, t, n)
{
    ggplot() +
        geom_line(aes(x = t[t < n], y = Z[t < n]), linewidth = 1) +
        geom_point(aes(x = n, y = Z[t == n]),
                   fill = "white", col = "black", pch = 21, size = 2) +
        geom_segment(aes(x = n, xend = t[length(t)], y = 0, yend = 0), linewidth = 1) +
        geom_point(aes(x = n, y = 0), size = 2) +
        labs(title = "Temporaire")
}
graph_Z_temp(the_Z, the_t, the_n)


graph_Z_cap_diff <- function(Z, t, n)
{
    cap_diff <- Z[t == n]
    ggplot() +
        geom_segment(aes(x = t[1], xend = t[which(t == n) - 1],
                         y = 0, yend = 0), linewidth = 1) +
        geom_point(aes(x = n, y = 0),
                   fill = "white", col = "black", pch = 21, size = 2) +
        geom_segment(aes(x = n, xend = t[length(t)],
                         y = cap_diff, yend = cap_diff), linewidth = 1) +
        geom_point(aes(x = n, y = cap_diff), size = 2) +
        labs(title = "Capital différé")
}
graph_Z_cap_diff(val, TT, 30)

graph_Z_mixte <- function(Z, t, n)
{
    cap_diff <- Z[t == n]
    ggplot() +
        geom_line(aes(x = t[t < n], y = Z[t < n]), linewidth = 1) +
        geom_segment(aes(x = n, xend = t[length(t)],
                         y = cap_diff, yend = cap_diff), linewidth = 1) +
        labs(title = "Mixte")
}
graph_Z_mixte(val, TT, 30)

graph_Z_diff <- function(Z, t, n)
{
    ggplot() +
        geom_segment(aes(x = t[1], xend = t[which(t == n) - 1],
                         y = 0, yend = 0), linewidth = 1) +
        geom_point(aes(x = n, y = 0),
                   fill = "white", col = "black", pch = 21, size = 2) +
        geom_line(aes(x = t[t >= n], y = Z[t >= n]), linewidth = 1) +
        geom_point(aes(x = n, y = Z[t == n]), size = 2) +
        labs(title = "Différée")
}
graph_Z_diff(val, TT, 30)

graph_Z <- function(..., x, type = c("ent", "temp", "cap_diff", "mixte", "diff"))
{
    graph <- switch(match.arg(type),
                    "ent" = graph_Z_ent(...),
                    "temp" = graph_Z_temp(...),
                    "cap_diff" = graph_Z_cap_diff(...),
                    "mixte" = graph_Z_mixte(...),
                    "diff" = graph_Z_diff(...))
    graph +
        labs(x = TeX(paste0("$T_{", x, "}$")),
             y = "Z") +
        scale_y_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0, 1)) +
        theme_bw()
}

graph_Z(Z = the_Z, t = the_t, n = the_n, x = the_x, type = "ent")
graph_Z(Z = the_Z, t = the_t, n = the_n, x = the_x, type = "temp")
graph_Z(Z = the_Z, t = the_t, n = the_n, x = the_x, type = "cap_diff")
graph_Z(Z = the_Z, t = the_t, n = the_n, x = the_x, type = "mixte")
graph_Z(Z = the_Z, t = the_t, n = the_n, x = the_x, type = "diff")

the_delta <- 0.07
the_omega <- 200
the_mu <- 0.03
the_x <- 0
the_n <- 30
the_t <- seq(0, the_omega - the_x, 0.1)
the_Z <- Z(the_delta, the_t)
the_tqx <- tqx("demoivre", t = the_t, omega = the_omega, x = the_x)
the_t <- seq(0, 100, 0.1)
the_tqx <- tqx("expo", t = the_t, mu = the_mu)


graph_FZ_ent <- function(Z, t, tqx, n)
{
    ggplot() +
        geom_line(aes(x = Z, y = 1 - tqx), linewidth = 1) +
        geom_segment(aes(x = -0.1, xend = Z[length(Z)], y = 0, yend = 0), linewidth = 1) +
        geom_segment(aes(x = 1, xend = 1.1, y = 1, yend = 1), linewidth = 1) +
        labs(title = "Fonction de répartition pour une assurance vie entière")
}

graph_FZ_temp <- function(Z, t, tqx, n)
{
    ggplot() +
        geom_line(aes(x = Z[t <= n], y = (1 - tqx)[t <= n]), linewidth = 1) +
        geom_segment(aes(x = 0, xend = Z[t == n], y = (1 - tqx)[t == n], yend = (1 - tqx)[t == n]), linewidth = 1) +
        geom_segment(aes(x = -0.1, xend = 0, y = 0, yend = 0), linewidth = 1) +
        geom_segment(aes(x = 1, xend = 1.1, y = 1, yend = 1), linewidth = 1) +
        geom_point(aes(x = 0, y = 0),
                   fill = "white", col = "black", pch = 21, size = 2) +
        geom_point(aes(x = 0, y = (1 - tqx)[t == n]), size = 2) +
        labs(title = "Fonction de répartition pour une assurance temporaire")
}

graph_FZ_cap_diff <- function(Z, t, tqx, n)
{
    cap_diff <- Z[t == n]
    ggplot() +
        geom_segment(aes(x = cap_diff, xend = 1.1, y = 1, yend = 1), linewidth = 1) +
        geom_segment(aes(x = 0, xend = cap_diff, y = tqx[t == n], yend = tqx[t == n]), linewidth = 1) +
        geom_segment(aes(x = -0.1, xend = 0, y = 0, yend = 0), linewidth = 1) +
        geom_point(aes(x = 0, y = 0),
                   fill = "white", col = "black", pch = 21, size = 2) +
        geom_point(aes(x = 0, y = tqx[t == n]), size = 2) +
        geom_point(aes(x = cap_diff, y = tqx[t == n]),
                   fill = "white", col = "black", pch = 21, size = 2) +
        geom_point(aes(x = cap_diff, y = 1), size = 2) +
        labs(title = "Fonction de répartition pour un capital différé")
}

graph_FZ_mixte <- function(Z, t, tqx, n)
{
    ggplot() +
        geom_line(aes(x = Z[t < n], y = (1 - tqx)[t < n]), linewidth = 1) +
        geom_segment(aes(x = -0.1, xend = Z[t == n], y = 0, yend = 0), linewidth = 1) +
        geom_segment(aes(x = 1, xend = 1.1, y = 1, yend = 1), linewidth = 1) +
        geom_point(aes(x = Z[t == n], y = 0),
                   fill = "white", col = "black", pch = 21, size = 2) +
        geom_point(aes(x = Z[t == n], y = (1 - tqx)[t == n]), size = 2) +
        labs(title = "Fonction de répartition pour une assurance mixte")
}

graph_FZ_diff <- function(Z, t, tqx, n)
{
    ggplot() +
        geom_segment(aes(x = Z[t == n], xend = 1.1, y = 1, yend = 1), linewidth = 1) +
        geom_line(aes(x = Z[t >= n], y = 1 + tqx[t == n] - tqx[t >= n]), linewidth = 1) +
        geom_segment(aes(x = -0.1, xend = Z[length(Z)], y = 0, yend = 0), linewidth = 1) +
        geom_point(aes(x = Z[length(Z)], y = 0),
                   fill = "white", col = "black", pch = 21, size = 2) +
        geom_point(aes(x = Z[length(Z)], y = tqx[t == n]), size = 2) +
        labs(title = "Fonction de répartition pour une assurance différée")
}

graph_FZ <- function(..., x, type = c("ent", "temp", "cap_diff", "mixte", "diff"))
{
    graph <- switch(match.arg(type),
                    "ent" = graph_FZ_ent(...),
                    "temp" = graph_FZ_temp(...),
                    "cap_diff" = graph_FZ_cap_diff(...),
                    "mixte" = graph_FZ_mixte(...),
                    "diff" = graph_FZ_diff(...))
    graph +
        labs(x = "x",
             y = TeX("$F_Z(x)$")) +
        scale_y_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0, 1)) +
        scale_x_continuous(breaks = seq(0, 1, by = 0.1), limits = c(-0.1, 1.1)) +
        theme_bw()
}

graph_FZ(Z = the_Z, t = the_t, tqx = the_tqx, n = the_n)
graph_FZ(Z = the_Z, t = the_t, tqx = the_tqx, n = the_n, type = "temp")
graph_FZ(Z = the_Z, t = the_t, tqx = the_tqx, n = the_n, type = "cap_diff")
graph_FZ(Z = the_Z, t = the_t, tqx = the_tqx, n = the_n, type = "mixte")
graph_FZ(Z = the_Z, t = the_t, tqx = the_tqx, n = the_n, type = "diff")
# Note : aller assez loin dans les âges pour connecter la courbe et le point



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













### Y


the_delta <- 0.1
the_omega <- 100
the_mu <- 0.03
the_x <- 0
the_n <- 10
the_t <- seq(0, the_omega - the_x, 0.1)
the_Y <- Y(the_delta, the_t)
the_tqx <- tqx("demoivre", t = the_t, omega = the_omega, x = the_x)
the_t <- seq(0, 100, 0.1)
the_tqx <- tqx("expo", t = the_t, mu = the_mu)
graph_Y_via(the_Y, the_t)

graph_Y(Y = the_Y, t = the_t, x = the_x)
graph_Y(Y = the_Y, t = the_t, n = the_n, x = the_x, type = "temp")
graph_Y(Y = the_Y, t = the_t, n = the_n, x = the_x, type = "diff")
graph_Y(Y = the_Y, t = the_t, n = the_n, x = the_x, type = "gar")
graph_FY(Y = the_Y, t = the_t, tqx = the_tqx, n = the_n,  x = the_x)
graph_FY(Y = the_Y, t = the_t, tqx = the_tqx, n = the_n,  x = the_x, type = "temp")
graph_FY(Y = the_Y, t = the_t, tqx = the_tqx, n = the_n,  x = the_x, type = "diff")
graph_FY(Y = the_Y, t = the_t, tqx = the_tqx, n = the_n,  x = the_x, type = "gar")

the_Y[the_t == the_n] - the_Y[the_t == the_n]

graph_FY_via(the_Y, the_t, the_tqx, the_n)
graph_FY_temp(the_Y, the_t, the_tqx, the_n)
the_Y[the_t == the_n]
the_tqx[the_t == the_n]
