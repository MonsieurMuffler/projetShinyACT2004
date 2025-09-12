#' @export
graph_FZ_mixte <- function(Z, t, tqx, n)
{
    ggplot() +
        geom_line(aes(x = Z[t <= n], y = (1 - tqx)[t <= n]), linewidth = 1) +
        geom_segment(aes(x = -0.1, xend = Z[t == n], y = 0, yend = 0), linewidth = 1) +
        geom_segment(aes(x = 1, xend = 1.1, y = 1, yend = 1), linewidth = 1) +
        geom_point(aes(x = Z[t == n], y = 0),
                   fill = "white", col = "black", pch = 21, size = 2) +
        geom_point(aes(x = Z[t == n], y = (1 - tqx)[t == n]), size = 2) +
        labs(title = "Fonction de répartition (assurance mixte)")
}