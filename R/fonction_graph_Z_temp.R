#' @export
graph_Z_temp <- function(Z, t, n)
{
    ggplot() +
        geom_line(aes(x = t[t < n], y = Z[t < n]), linewidth = 1) +
        geom_point(aes(x = n, y = Z[t == n]),
                   fill = "white", col = "black", pch = 21, size = 2) +
        geom_segment(aes(x = n, xend = t[length(t)], y = 0, yend = 0), linewidth = 1) +
        geom_point(aes(x = n, y = 0), size = 2) +
        labs(title = "Assurance temporaire")
}
