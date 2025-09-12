#' @export
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
        labs(title = "Fonction de répartition (capital différé)")
}