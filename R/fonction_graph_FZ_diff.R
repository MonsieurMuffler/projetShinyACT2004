#' @export
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