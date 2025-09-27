#' @export
graph_FY_temp <- function(Y, t, tqx, n)
{
    ggplot() +
        geom_line(aes(x = Y[t <= n], y = tqx[t <= n]), linewidth = 1) +
        geom_segment(aes(x = -0.5, xend = 0, y = 0, yend = 0), linewidth = 1) +
        geom_segment(aes(x = Y[t == n], xend = max(Y) * 1.05, y = 1, yend = 1), linewidth = 1) +
        geom_point(aes(x = Y[t == n], y = tqx[t == n]),
                   fill = "white", col = "black", pch = 21, size = 2) +
        geom_point(aes(x = Y[t == n], y = 1), size = 2) +
        labs(title = "Fonction de répartition (rente temporaire)")
}
