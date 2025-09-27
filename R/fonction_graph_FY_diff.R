#' @export
graph_FY_diff <- function(Y, t, tqx, n)
{
    Y_mod <- Y[t >= n] - Y[t == n]
    ggplot() +
        geom_line(aes(x = Y_mod, y = tqx[t >= n]), linewidth = 1) +
        geom_segment(aes(x = -0.5, xend = 0, y = 0, yend = 0), linewidth = 1) +
        geom_segment(aes(x = max(Y_mod), xend = max(Y) * 1.05, y = 1, yend = 1), linewidth = 1) +
        geom_point(aes(x = 0, y = 0),
                   fill = "white", col = "black", pch = 21, size = 2) +
        geom_point(aes(x = 0, y = tqx[t == n]), size = 2) +
        labs(title = "Fonction de répartition (rente différée)")
}
