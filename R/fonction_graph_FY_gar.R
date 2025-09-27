#' @export
graph_FY_gar <- function(Y, t, tqx, n)
{
    ggplot() +
        geom_line(aes(x = Y[t >= n], y = tqx[t >= n]), linewidth = 1) +
        geom_segment(aes(x = -0.5, xend = Y[t == n], y = 0, yend = 0), linewidth = 1) +
        geom_segment(aes(x = max(Y), xend = max(Y) * 1.05, y = 1, yend = 1), linewidth = 1) +
        geom_point(aes(x = Y[t == n], y = 0),
                   fill = "white", col = "black", pch = 21, size = 2) +
        geom_point(aes(x = Y[t == n], y = tqx[t == n]), size = 2) +
        labs(title = "Fonction de répartition (rente avec garantie)")
}
