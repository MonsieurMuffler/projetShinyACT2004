#' @export
graph_Y_gar <- function(Y, t, n)
{
    ggplot() +
        geom_segment(aes(x = 0, xend = n, y = Y[t == n], yend = Y[t == n]), linewidth = 1) +
        geom_line(aes(x = t[t >= n], y = Y[t >= n]), linewidth = 1) +
        labs(title = "Valeur actualisée (rente avec garantie)")
}
