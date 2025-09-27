#' @export
graph_Y_temp <- function(Y, t, n)
{
    ggplot() +
        geom_line(aes(x = t[t <= n], y = Y[t <= n]), linewidth = 1) +
        geom_segment(aes(x = n, xend = t[length(t)], y = Y[t == n], yend = Y[t == n]), linewidth = 1) +
        labs(title = "Valeur actualisée (rente temporaire)")
}
