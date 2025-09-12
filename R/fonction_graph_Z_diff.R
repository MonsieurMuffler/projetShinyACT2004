#' @export
graph_Z_diff <- function(Z, t, n)
{
    ggplot() +
        geom_segment(aes(x = t[1], xend = t[which(t == n) - 1],
                         y = 0, yend = 0), linewidth = 1) +
        geom_point(aes(x = n, y = 0),
                   fill = "white", col = "black", pch = 21, size = 2) +
        geom_line(aes(x = t[t >= n], y = Z[t >= n]), linewidth = 1) +
        geom_point(aes(x = n, y = Z[t == n]), size = 2) +
        labs(title = "Valeur actualisée (assurance différée)")
}
